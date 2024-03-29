{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to an RSI analysis.

module Skel.RSI.Main where

import DataAnalysis.Application.Prelude
import Skel.RSI.UserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "RSI analysis" userAnalysis
