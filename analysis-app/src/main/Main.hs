{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | Main entry point to my analysis app.

module Main where

import           DataAnalysis.Application.Prelude
import           DataAnalysis.Application.Types
import           DataAnalysis.Application.Types.Stock

import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.Analysis                as CA
import qualified Data.Conduit.List                    as CL
import           Data.Default
import           Data.Proxy
import           Data.Text                            (pack)
import qualified Data.Vector                          as V
import           Yesod

-- | Parameters to the analysis.
data RsiParams = RsiParams
    { rpSize  :: !Int -- ^ RSI grouping size
    , rpAlpha :: !Double -- ^ alpha for exponential moving average
    , rpBars  :: !Int -- ^ number of bars to display
    }

-- | Implement an analysis for the RsiParams type.
instance HasAnalysis RsiParams where
    type AnalysisInput RsiParams = Stock

    analysisOf (RsiParams size alpha bars) =
            stocksToUpDown
        =$= CA.groupsOf size -- FIXME: need a better name, movingGroupsOf?
        =$= CL.map (calculateRSI alpha)
        =$= CL.isolate bars

-- | Calculate the RSI value.
calculateRSI
    :: Double -- ^ alpha
    -> V.Vector UpDown
    -> DataPoint
calculateRSI alpha v =
    DP (pack (show (V.head v ^. udDay))) rsi' Nothing
  where
    rs = CA.exponentialMovingAverage udUp   alpha v
       / CA.exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm RsiParams where
    form = RsiParams
        <$> areq intField "Grouping size" (Just (rpSize def))
        <*> areq doubleField "Alpha (for exponential moving average)" (Just (rpAlpha def))
        <*> areq intField "Number of bars (up to 20 for a readable graph)" (Just (rpBars def))

-- | Default values for the parameters.
instance Default RsiParams where
  def = RsiParams 14
                  0.6
                  20

-- | Start the analysis server with the following configuration.
main :: IO ()
main =
  runAnalysisApp "RSI analysis"
                 (Proxy :: Proxy RsiParams)
