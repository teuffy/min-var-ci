{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to my analysis app.

module Skel.Kmer.Main where

import DataAnalysis.Application.Prelude
import Skel.Kmer.UserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisAppRaw "Kmer analysis" userAnalysis
