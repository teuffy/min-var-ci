{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module DataAnalysis.Application.Import
    ( module DataAnalysis.Application.Import
    , module X
    ) where

import           Control.Applicative                  as X
import           Control.Lens                         as X
import           Data.ByteString                      (ByteString)
import           Data.Conduit                         as X
import           Data.Conduit.Analysis                as X
import           Data.Conduit.List                    as X (isolate)
import qualified Data.Conduit.List                    as CL
import           Data.Default                         as X
import qualified Data.Map                             as Map
import           Data.Text                            (Text, pack, unpack)
import           Data.Time                            (Day)
import           Data.Vector                          as X (Vector)
import           DataAnalysis.Application.Types       as X
import           DataAnalysis.Application.Types.Stock as X
import           Safe                                 (readMay)
import           Yesod                                as X hiding ((.=), (<.))

data UpDown = UpDown
    { _udDay  :: !Day
    , _udUp   :: !Double
    , _udDown :: !Double
    }
    deriving Show
makeClassy ''UpDown

stocksToUpDown :: Monad m => Conduit Stock m UpDown
stocksToUpDown =
    await >>= maybe (return ()) loop
  where
    loop today = do
        myesterday <- await
        case myesterday of
            Nothing -> return ()
            Just yesterday -> do
                let ud = UpDown
                        { _udDay = today ^. stockDay
                        , _udUp = max 0 $ (today ^. stockAdjClose) - (yesterday ^. stockAdjClose)
                        , _udDown = max 0 $ (yesterday ^. stockAdjClose) - (today ^. stockAdjClose)
                        }
                yield ud
                loop yesterday

rawStockSource :: (MonadResource m, MonadBaseControl IO m, ManagerReader m)
               => Text -- ^ stock symbol
               -> Source m ByteString
rawStockSource symbol = sourceURL $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ unpack symbol

shown :: Show a => IndexPreservingGetter a Text
shown = to (pack . show)

mapStream :: Monad m => (a -> b) -> Conduit a m b
mapStream = CL.map

