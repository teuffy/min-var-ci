-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Custom.UserAnalysis (userAnalysis) where

import           Skel.Custom.UserParameters

userAnalysis :: MonadIO m => CustomParams -> Conduit Stock m DataPoint
userAnalysis _ =
    -- Replace with your analysis code here
    return ()
