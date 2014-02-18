-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.RSI.UserAnalysis (userAnalysis) where

import           Skel.RSI.UserParameters

-- The following is an implementation of relative strength index.
-- For more information, see: http://en.wikipedia.org/wiki/Relative_strength_index

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.

-- For a list of commonly used functions to use in an analysis, see:
--
-- http://download.fpcomplete.com/tempdocs/data-analysis-library/DataAnalysis-Library.html
userAnalysis :: RsiParams -> Conduit Stock Analysis DataPoint
userAnalysis (RsiParams size alpha bars) =
    -- begin the analysis
        startAnalysis

    -- perform some custom filtering
    -- to disable custom filtering, comment out the following line
    =$= applyCustomFilter userFilter

    -- compute the change in stock price on two consecutive days
    =$= stocksToUpDown stockDate stockAdjClose

    -- group the changes into vectors of the user-specified size
    =$= movingGroupsOf size

    -- perform the RSI calculation on each packed vector
    =$= mapStream (calculateRSI alpha)

    -- limit output to the number of data points requested by the user
    -- note that no unnecessary computations will be performed
    =$= isolate bars

-- | Calculate the RSI value.
calculateRSI
    :: Double -- ^ alpha
    -> (UpDown, Vector UpDown)
    -> DataPoint
calculateRSI alpha (firstUpDown, v) =
    DP2 (D2D (firstUpDown ^. udDate . shown) rsi' Nothing)
  where
    rs = exponentialMovingAverage udUp   alpha v
       / exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))

userFilter :: Stock -> FilterAction Stock
userFilter stock
    -- Discard any records in the year 2014 or later
    | stock ^. stockDate >= "2014-01-01"
        = DropRecord "Only dealing with 2013 and earlier"
    -- If the adjusted close is negative, take its absolute value
    | stock ^. stockAdjClose < 0
        = ReplaceWith "Fixed negative close price"
        $ stock & (stockAdjClose %~ abs)
    -- If high is less than low, then the record is faulty and discard
    | stock ^. stockHigh < stock ^. stockLow
        = DropRecord "High is less than low"
    -- No problems with the record, keep it
    | otherwise = KeepRecord
