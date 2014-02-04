{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- | A library of commonly used data analysis functions.
--
-- Note that this is an initial, demo version of the library. The final version
-- will be much more fully featured.
module DataAnalysis.Library
    ( -- * General purpose
      -- ** Streaming functions
      -- $streaming
      mapStream
    , mapField
    , filterStream
    , filterField
    , sumStream
    , sumField
    , movingGroupsOf
      -- ** Lens helpers
    , shown
      -- * Mathematical
    , exponentialMovingAverage
      -- * Financial
      -- ** Price differentials
    , UpDown (..)
    , stocksToUpDown
      -- *** Lenses
    , HasUpDown (..)

      -- * Genome
      -- ** K-mer counting
      -- *** Types
    , Nucleobases
    , Kmers
    , KmerHistogram
      -- *** Streaming functions
    , parseFasta
    , conduitKmers
    , kmerHistogram
    , regroupKmers
    , kmerDataPoint
    ) where

-- Nota bene! The documentation for this module will be generated as Haddocks
-- and given to users, so it must be kept clean and understandable.

import           Control.Applicative            ((<$>), (<*>))
import           Control.Lens
import           Control.Monad                  (unless, when)
import           Control.Monad.Base
import           Control.Monad.Primitive        (PrimMonad)
import           Data.Bits                      (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString                as S
import           Data.Conduit
import           Data.Conduit.Analysis
import qualified Data.Conduit.List              as CL
import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack)
import qualified Data.Text                      as T
import           Data.Time                      (Day)
import qualified Data.Vector.Primitive.Mutable  as V
import           Data.Word                      (Word8)
import           DataAnalysis.Application.Types (DataPoint (..))

#if MIN_VERSION_conduit(1, 0, 12)
import qualified Data.Conduit.Binary            as CB
#endif

-- $streaming
--
-- The following functions come in two flavors: those operating on the streamed
-- value itself, and those operating on a single field of the streamed value.
-- The latter are convenience functions to make many kinds of common analysis
-- simple, such as taking the logarithm of a specific field in a data
-- structure.

-- | Perform the given transformation on all values in the stream.
--
-- @
-- yieldMany [1..10] =$= mapStream (+1) =$= sumStream
-- @
mapStream :: Monad m => (a -> b) -> Conduit a m b
mapStream = CL.map

-- | Perform the given transformation on the given field in the stream.
--
-- @
-- yieldMany (zip [1..10] [11..20]) =$= mapField _1 (+1) =$= sumField _1
-- @
mapField :: Monad m => Lens s t a b -> (a -> b) -> Conduit s m t
mapField field f = CL.map (field %~ f)

-- | Keep only the values from the stream passing the given predicate function.
--
-- @
-- yieldMany [1..10] =$= filterStream even =$= sumStream
-- @
filterStream :: Monad m => (a -> Bool) -> Conduit a m a
filterStream = CL.filter

-- | Keep only the values from the stream where the given field passes the
-- given predicate function.
--
-- @
-- yieldMany ([1..10], [11..20]) =$= filterField _1 even =$= sumField _2
-- @
filterField :: Monad m => Lens' s a -> (a -> Bool) -> Conduit s m s
filterField field f = CL.filter (f . view field)

-- | Sum all of the values in a stream.
sumStream :: (Monad m, Num a) => Consumer a m a
sumStream = CL.fold (+) 0

-- | Sum up a specific field in a stream.
sumField :: (Monad m, Num a) => Getter s a -> Consumer s m a
sumField field = CL.fold (\total s -> total + (s ^. field)) 0

-- | Convert a value to its textual representation.
--
-- Uses the @Show@ instance for the type.
shown :: Show a => IndexPreservingGetter a Text
shown = to (pack . show)

-- | The difference either up or down of a stock price from one day to the
-- next.
data UpDown = UpDown
    { _udDate :: !Day
    , _udUp   :: !Double
    , _udDown :: !Double
    }
    deriving Show
makeClassy ''UpDown

-- | Convert a stream of stock prices to a stream of up/down values.
--
-- You must provide the names of the date and adjusted close price fields.
stocksToUpDown :: Monad m
               => Getter stock Day    -- ^ date field
               -> Getter stock Double -- ^ adjusted close
               -> Conduit stock m UpDown
stocksToUpDown stockDate stockAdjClose =
    await >>= maybe (return ()) loop
  where
    loop today = do
        myesterday <- await
        case myesterday of
            Nothing -> return ()
            Just yesterday -> do
                let ud = UpDown
                        { _udDate = today ^. stockDate
                        , _udUp = max 0 $ (today ^. stockAdjClose) - (yesterday ^. stockAdjClose)
                        , _udDown = max 0 $ (yesterday ^. stockAdjClose) - (today ^. stockAdjClose)
                        }
                yield ud
                loop yesterday

type Base = Word8
type Kmer4 = Word8

toBase :: Word8 -> Base
toBase 65 = 0
toBase 67 = 1
toBase 71 = 2
toBase 84 = 3
toBase w = error $ "Invalid base: " ++ show w

mkKmer4 :: Base -> Base -> Base -> Base -> Kmer4
mkKmer4 w x y z = shiftL w 6
              .|. shiftL x 4
              .|. shiftL y 2
              .|.        z

showKmer4 :: Int -> T.Text
showKmer4 =
    T.pack . loop (4 :: Int) []
  where
    loop 0 x _ = x
    loop i x y =
        loop (i - 1) (toChar y1 : x) y2
      where
        y1 = y .&. 3
        y2 = shiftR y 2

    toChar 0 = 'A'
    toChar 1 = 'C'
    toChar 2 = 'G'
    toChar 3 = 'T'
    toChar i = error $ "showKmer4: " ++ show i

data KmerHistogram = KmerHistogram !T.Text !Int
    deriving (Show, Eq, Ord, Read)

-- | Regroup k-mers using the given textual representation conversion.
--
-- @
-- regroupKmers (T.take 2)
-- @
regroupKmers :: Monad m => (T.Text -> T.Text) -> Conduit KmerHistogram m KmerHistogram
regroupKmers f =
    loop Map.empty
  where
    loop m = await >>= maybe (finish m) (go m)

    go m (KmerHistogram t i) = loop $ Map.alter (Just . maybe i (+ i)) (f t) m

    finish = mapM_ (\(k, v) -> yield $ KmerHistogram k v) . Map.toList

mapM_BS :: Monad m => (Word8 -> m ()) -> Consumer S.ByteString m ()
#if MIN_VERSION_conduit(1, 0, 12)
mapM_BS = CB.mapM_
#else
mapM_BS f = CL.mapM_ $ mapM_ f . S.unpack
#endif

-- | Count frequencies
kmerHistogram :: (MonadBase base m, PrimMonad base) => Conduit Kmers m KmerHistogram
kmerHistogram = do
    v <- liftBase $ V.replicate 256 0
    mapInput unKmers (Just . Kmers) $ mapM_BS $ \k' -> liftBase $ do
        let k = fromIntegral k'
        i <- V.read v k
        V.write v k (i + 1)
    let loop 256 = return ()
        loop k = do
            i <- liftBase $ V.read v k
            when (i > 0) $ yield $ KmerHistogram (showKmer4 k) i
            loop (k + 1)
    loop 0

-- | A packed representation of four-gram kmers.
newtype Kmers = Kmers { unKmers :: S.ByteString }

-- | Convert a stream of individual nucleobases into a stream of k-mers of size
-- 4.
conduitKmers :: Monad m => Conduit Nucleobases m Kmers
conduitKmers = do
    mb1 <- head'
    mb2 <- head'
    mb3 <- head'
    case (,,) <$> mb1 <*> mb2 <*> mb3 of
        Nothing -> return ()
        Just accum -> loop accum
  where
    step (b1, b2, b3) b4 = ((b2, b3, b4), mkKmer4 b1 b2 b3 b4)

    head' = await >>= maybe (return Nothing) (\(Nucleobases bs) ->
        case S.uncons bs of
            Nothing -> head'
            Just (w, bs') -> leftover (Nucleobases bs') >> return (Just w))

    loop accum = await >>= maybe (return ()) (go accum)

    go accum (Nucleobases bs) = do
        let (accum', bs') = S.mapAccumL step accum bs
        yield $ Kmers bs'
        loop accum'

-- | Packed representation of the four nucleobases.
newtype Nucleobases = Nucleobases S.ByteString

-- | Convert an incoming stream of raw bytes in FASTA format into a packed
-- representation of nucleobases.
parseFasta :: Monad m => Conduit S.ByteString m Nucleobases
parseFasta =
    startOn
  where
    toBases = S.map toBase . S.filter (\w -> w == 65 || w == 67 || w == 71 || w == 84)
    yieldBases x = do
        let x' = toBases x
        unless (S.null x') (yield $ Nucleobases x')

    startOn = await >>= maybe (return ()) goOn

    goOn bs = do
        yieldBases x
        if S.null y
            then startOn
            else leftover y >> startOff
      where
        (x, y) = S.breakByte 62 bs

    startOff = await >>= maybe (return ()) goOff

    goOff bs
        | S.null y = startOff
        | otherwise = leftover y >> startOn
      where
        (_, y) = S.breakByte 10 bs

-- | Convert a stream of kmer histograms into data points for output.
kmerDataPoint :: Monad m => Conduit KmerHistogram m DataPoint
kmerDataPoint =
    CL.map go
  where
    go (KmerHistogram t i) = DP t (fromIntegral i) Nothing
