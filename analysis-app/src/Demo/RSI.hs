{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
module Demo.RSI where

import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.Analysis                as CA
import qualified Data.Conduit.List                    as CL
import qualified Data.Map                             as Map
import           Data.Proxy
import           Data.Text                            (Text, pack)
import           Data.Time                            (Day)
import qualified Data.Vector                          as V
import           DataAnalysis.Application.Types.Stock
import           Demo.Common
import           Demo.Helper.Class
import           Yesod

data RSI = RSI
    { rsiDay   :: !Day
    , rsiValue :: !Double
    }
    deriving Show

calculateRSI
    :: Double -- ^ alpha
    -> V.Vector UpDown
    -> RSI
calculateRSI alpha v =
    RSI (V.head v ^. udDay) rsi'
  where
    rs = CA.exponentialMovingAverage udUp   alpha v
       / CA.exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))

instance ToMapRow RSI where
    toMapRow rsi = Map.fromList
        [ ("date", pack $ show $ rsiDay rsi)
        , ("rsi",  pack $ show $ rsiValue rsi)
        ]

data RsiParams = RsiParams
    { rpSymbol :: !Text -- ^ stock symbol
    , rpSize   :: !Int -- ^ RSI grouping size
    , rpAlpha  :: !Double -- ^ alpha for exponential moving average
    }

instance HasAnalysis RsiParams where
    type AnalysisInput RsiParams = Stock
    type AnalysisOutput RsiParams = RSI

    analysisOf (RsiParams _ size alpha) =
            stocksToUpDown
        =$= CA.groupsOf size -- FIXME: need a better name, movingGroupsOf?
        =$= CL.map (calculateRSI alpha)

    analysisInput (RsiParams symbol _ _) = rawStockSource symbol

instance HasForm RsiParams where
    form = RsiParams
        <$> areq textField "Stock Symbol" (Just "YHOO")
        <*> areq intField "Grouping size" (Just 14)
        <*> areq doubleField "Alpha (for exponential moving average)" (Just 0.6)

main :: IO ()
main = launch (Proxy :: Proxy RsiParams)
