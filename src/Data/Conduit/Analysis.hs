{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Data.Conduit.Analysis where

import Data.Conduit
import Data.List (sort)
import qualified Data.Conduit.Internal as CI
import Data.Conduit.Internal (Pipe (..))
import qualified Control.Lens as L
import Data.Hashable (Hashable)
import Data.Word (Word64)
import qualified Data.Conduit.List as CL
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Foldable (Foldable, forM_)
import Data.Traversable (Traversable)
import qualified Data.List
import Data.Ord (comparing)
import qualified Data.IORef as I
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Applicative ((<$>), (<*>))

data GroupByStrategy
    = PreSorted
    | SortThenGroup
    | Coroutines
    | Threads

data ConduitManager a i m o = ConduitManager
    { cmFeed :: i -> a -> Conduit i m o
    , cmCloseAll :: Conduit i m o
    }

newCoroutineManager :: (Eq k, MonadIO m, Hashable k)
                    => (i -> k -> Conduit i m o)
                    -> IO (ConduitManager k i m o)
newCoroutineManager inner = do
    imap <- I.newIORef HM.empty
    return ConduitManager
        { cmFeed = \i a -> do
            m <- liftIO $ I.readIORef imap
            x <-
                case HM.lookup a m of
                    Nothing -> return (CI.unConduitM $ inner i a, return ())
                    Just x -> return x
            x' <- feed x [i]
            liftIO $ I.writeIORef imap $ HM.insert a x' m
        , cmCloseAll = do
            m <- liftIO $ I.readIORef imap
            forM_ m closeOne
        }
  where
    feed (pipe, finalizer) is =
        case pipe of
            Done () -> return (Done (), return ())
            PipeM m -> do
                pipe' <- lift m
                feed (pipe', finalizer) is
            Leftover pipe' i -> feed (pipe', finalizer) (i:is)
            HaveOutput pipe' finalizer' o -> do
                yield o
                feed (pipe', finalizer') is
            pipe'@(NeedInput f _) ->
                case is of
                    ifirst:irest -> feed (f ifirst, finalizer) irest
                    [] -> return (pipe', finalizer)

    closeOne (pipe, finalizer) =
        case pipe of
            Done () -> return ()
            PipeM m -> do
                pipe' <- lift m
                closeOne (pipe', finalizer)
            Leftover pipe' i -> feed (pipe', finalizer) [i] >>= closeOne
            HaveOutput pipe' finalizer' o -> do
                yield o
                closeOne (pipe', finalizer')
            NeedInput _ c -> closeOne (c (), finalizer)

groupBy :: (Ord a, Hashable a, MonadResource m)
        => GroupByStrategy
        -> L.Getter i a
        -> (i -> a -> Conduit i m o)
        -> Conduit i m o
groupBy PreSorted lens inner =
    start
  where
    start = do
        mi <- await
        case mi of
            Nothing -> return ()
            Just i -> do
                let a = i L.^. lens
                leftover i
                isolateTo a =$= inner i a
                start
    isolateTo a = do
        mi <- await
        case mi of
            Nothing -> return ()
            Just i
                | i L.^. lens == a -> do
                    yield i
                    isolateTo a
                | otherwise -> leftover i
groupBy SortThenGroup lens inner = do
    vals <- CL.consume
    let sorted = Data.List.sortBy (comparing (L.^. lens)) vals
    mapM_ yield sorted =$= groupBy PreSorted lens inner
groupBy Coroutines lens inner = do
    manager <- liftIO $ newCoroutineManager inner
    awaitForever $ \i ->
        let a = i L.^. lens
         in cmFeed manager i a
    cmCloseAll manager
groupBy Threads _ _ = error "groupBy Threads not yet implemented"

data Stats a = Stats
    { statsCount :: !Word64
    , statsTotal :: !a
    , statsSquareTotal :: !a
    }
    deriving (Functor, Foldable, Traversable)
instance Num a => Semigroup (Stats a) where
    Stats c1 t1 s1 <> Stats c2 t2 s2 = Stats (c1 + c2) (t1 + t2) (s1 + s2)

instance Num a => Monoid (Stats a) where
    mempty = Stats 0 0 0
    mappend = (<>)

statsMean :: Fractional a => Stats a -> Maybe a
statsMean s
    | statsCount s == 0 = Nothing
    | otherwise = Just $ statsTotal s / fromIntegral (statsCount s)

statsSquareMean :: Fractional a => Stats a -> Maybe a
statsSquareMean s
    | statsCount s == 0 = Nothing
    | otherwise = Just $ statsSquareTotal s / fromIntegral (statsCount s)

statsStdDev :: Floating a => Stats a -> Maybe a
statsStdDev s = do
    ex <- statsMean s
    ex2 <- statsSquareMean s
    return $ sqrt $ ex2 - (ex * ex)

stats :: (Num a, Monad m)
      => L.Getter i a
      -> Consumer i m (Stats a)
stats lens =
    CL.foldMap go
  where
    go i = Stats
        { statsCount = 1
        , statsTotal = a
        , statsSquareTotal = a * a
        }
      where
        a = i L.^. lens

-- | Pack n incoming values into a vector and yield it.
-- Then read the subsequence value in the stream, drop the
-- first value from the vector, append the new value to the end,
-- and yield the new vector. Continue until the input stream is empty.
--
-- For example, if n is 3, and the input is [1, 2, 3, 4, 5], the yielded
-- values would be [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
movingGroupsOf :: MonadIO m => Int -> Conduit a m (a, V.Vector a)
movingGroupsOf size | size <= 0 = error "movingGroupsOf size must be greater than 0"
movingGroupsOf size = do
    mv <- liftIO $ M.new size
    fill 0 mv
    return ()
  where
    fill i mv
        | i == size = do
            v <- liftIO $ V.freeze mv
            yield (V.head v, v)
            continue v
        | otherwise = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just x -> do
                    liftIO $ M.write mv i x
                    fill (succ i) mv

    continue v = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x -> do
                let v' = V.drop 1 v `V.snoc` x
                yield (V.head v', v')
                continue v'

-- | Compute the exponential moving average of the given vector of values.
--
-- Precondition: the provided vector must be non-null.
exponentialMovingAverage
    :: Num a
    => L.Getter i a -- ^ field to average
    -> a -- ^ alpha
    -> V.Vector i
    -> a
exponentialMovingAverage lens alpha input
    | V.null input = error "Called exponentialMovingAverage with null vector"
    | otherwise = V.foldl1' step $ fmap (L.^. lens) input
  where
    step accum val = alpha * val + (1 - alpha) * accum

smoothDeltaAbnormalities
    :: (Monad m, Floating a, Ord a)
    => L.Lens' i a -- ^ field to correct
    -> Conduit i m i
smoothDeltaAbnormalities lens = do
    vals <- CL.take 20
    mapM_ leftover $ reverse vals
    if length vals < 6
        then CL.map id
        else do
            let deltas = drop 2 $ reverse $ drop 2 $ sort $ toDeltas $ map (L.view lens) vals
            s <- mapM_ yield deltas $$ stats id
            case (,) <$> statsMean s <*> statsStdDev s of
                Nothing -> CL.map id
                Just (mean, stddev) -> smooth mean stddev
  where
    toDeltas [] = []
    toDeltas (x:xs) =
        go x xs
      where
        go x (y:z) = abs (x - y) : go y z
        go _ [] = []

    smooth mean stddev =
        await >>= maybe (return ()) (\x -> yield x >> loop x)
      where
        loop old = do
            mnext <- await
            mafter <- CL.peek
            case (mnext, mafter) of
                (Nothing, Nothing) -> return ()
                (Nothing, Just _) -> return () -- cannot occur
                (Just next, Nothing) -> yield next
                (Just next, Just after) -> do
                    let delta1 = abs $ old L.^. lens - next L.^. lens
                        delta2 = abs $ after L.^. lens - next L.^. lens
                        next'
                            | delta1 > highDelta && delta2 > highDelta =
                                let replacement = (old L.^. lens + after L.^. lens) / 2
                                 in L.set lens replacement next
                            | otherwise = next
                    yield next'
                    loop next'

        highDelta = mean + 3 * stddev