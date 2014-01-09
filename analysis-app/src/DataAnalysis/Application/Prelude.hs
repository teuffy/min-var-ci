{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (module DataAnalysis.Application.Types
  ,module Data.Default
  ,module Data.Monoid
  ,module Yesod
  ,runAnalysisApp)
  where

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()
import DataAnalysis.Application.Foundation

import Control.Concurrent.STM
import Data.Default
import Data.Monoid

import Yesod

-- | Run the analysis web app.
runAnalysisApp :: Default params => AnalysisAppConfig params source -> IO ()
runAnalysisApp config = do
  tstore <- atomically $ newTVar mempty
  tident <- atomically $ newTVar 0
  warpEnv $ GApp (app tstore tident)
  where app tstore tident =
          App (analysisParser config)
              (analysisPrint config)
              (analysisFunc config)
              tident
              tstore
              (analysisTitle config)
              def
