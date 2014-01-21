-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module UserAnalysis (userAnalysis) where

import           UserParameters

-- The following is an implementation of relative strength index.
-- For more information, see: http://en.wikipedia.org/wiki/Relative_strength_index

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.
userAnalysis :: MonadIO m => RsiParams -> Conduit Stock m DataPoint
userAnalysis (RsiParams size alpha bars) =
        stocksToUpDown                 -- compute the change in stock price on two consecutive days
    =$= movingGroupsOf size            -- group the changes into vectors of the user-specified size
    =$= mapStream (calculateRSI alpha) -- perform the RSI calculation on each packed vector
    =$= isolate bars                   -- limit output to the number of data points requested by the user
                                       -- note that no unnecessary computations will be performed

-- | Calculate the RSI value.
calculateRSI
    :: Double -- ^ alpha
    -> (UpDown, Vector UpDown)
    -> DataPoint
calculateRSI alpha (firstUpDown, v) =
    DP (firstUpDown ^. udDay . shown) rsi' Nothing
  where
    rs = exponentialMovingAverage udUp   alpha v
       / exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))
