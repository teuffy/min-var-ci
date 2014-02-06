{-# LANGUAGE OverloadedStrings #-}

-- | Pharma analysis with 3D nurbs.

module Skel.Pharma.Main where

import DataAnalysis.Application.Prelude
import Skel.Pharma.UserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "Pharma" userAnalysis
