{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Demo.RSI where

import Data.Conduit
import Yesod
import           Control.Lens
import qualified Data.Conduit.Binary   as CB
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Text     as CT
import           Data.CSV.Conduit
import           Data.Time             (Day)
import qualified Data.Conduit.Analysis as CA
import qualified Data.Vector as V
import Network.HTTP.Conduit hiding (Proxy)
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (Builder, fromByteString)
import Control.Exception (Exception)
import Control.Applicative
import Data.Text (Text, unpack, pack)
import Data.Typeable (Typeable)
import Safe (readMay)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Demo.Helper.Class
import Data.Proxy
import Demo.Common
import DataAnalysis.Application.Types.Stock

data RSI = RSI
    { _rsiDay :: !Day
    , _rsiValue :: !Double
    }
    deriving Show
makeClassy ''RSI

rsi :: Double -- ^ alpha
    -> V.Vector UpDown -> RSI
rsi alpha v =
    RSI (V.head v ^. udDay) rsi'
  where
    rs = CA.exponentialMovingAverage udUp   alpha v
       / CA.exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))

instance ToMapRow RSI where
    toMapRow rsi = Map.fromList
        [ ("date", pack $ show $ rsi ^. rsiDay)
        , ("rsi", pack $ show $ rsi ^. rsiValue)
        ]

data RsiParams = RsiParams
    { rpSymbol :: !Text -- ^ stock symbol
    , rpSize :: !Int -- ^ RSI grouping size
    , rpAlpha :: !Double -- ^ alpha for exponential moving average
    }

instance HasAnalysis RsiParams where
    type AnalysisInput RsiParams = Stock
    type AnalysisOutput RsiParams = RSI

    analysisOf (RsiParams _ size alpha) =
            stocksToUpDown
        =$= CA.groupsOf size -- FIXME: need a better name, movingGroupsOf?
        =$= CL.map (rsi alpha)

    analysisInput (RsiParams symbol _ _) = rawStockSource symbol

instance HasForm RsiParams where
    form = RsiParams
        <$> areq textField "Stock Symbol" (Just "YHOO")
        <*> areq intField "Grouping size" (Just 14)
        <*> areq doubleField "Alpha (for exponential moving average)" (Just 0.6)

main :: IO ()
main = launch (Proxy :: Proxy RsiParams)