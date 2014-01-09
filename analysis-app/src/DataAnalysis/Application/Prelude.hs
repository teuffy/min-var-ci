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
import Data.Dynamic
import Data.Monoid

import Yesod

-- | Run the analysis web app.
runAnalysisApp :: (Typeable params,Typeable source) => AnalysisAppConfig params source -> IO ()
runAnalysisApp config = do
  tstore <- atomically $ newTVar mempty
  tident <- atomically $ newTVar 0
  warpEnv $
    App (fmap (fmap (fmap toDyn)) . analysisParser config)
        (maybe "Failed to convert type." (analysisPrint config) . fromDynamic)
        (\paramdyn dyns ->
           case fromDynamic paramdyn of
             Nothing -> error "Failed to convert parameters."
             Just ps ->
               case mapM fromDynamic dyns of
                 Nothing -> error "Failed to convert values."
                 Just xs -> analysisFunc config ps xs)
        tident
        tstore
        (analysisTitle config)
