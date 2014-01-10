{-# LANGUAGE RecordWildCards #-}

-- | Main entry point to my analysis app.

module Main where

import DataAnalysis.Application.Prelude

import Analysis (analysis)
import Params (params)
import Parser (parseCSVSource)
import Types (Parameters,Listen)

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp (def :: AnalysisAppConfig Parameters Listen)
  { analysisFunc = analysis
  , analysisParser = parseCSVSource -- Temporary, proper source will
                                    -- be a persistent thing written
                                    -- by Manny.
  , analysisForm = params
  }
