{-# LANGUAGE OverloadedStrings #-}

-- | Pharma analysis with 3D nurbs.

module Main where

import DataAnalysis.Application.Prelude
import PharmaUserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "Pharma" userAnalysis
