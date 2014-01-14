{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (module DataAnalysis.Application.Types
  ,module Data.Default
  ,module Data.Monoid
  ,module Yesod
  ,runAnalysisApp)
  where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.CSV.Conduit
import Data.Conduit (Conduit)
import Data.Default
import Data.Monoid
import Yesod
import Yesod.Static

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

-- | Run the analysis web app.
runAnalysisApp :: forall params source.
                  (Default params,CSV ByteString source) => AnalysisAppConfig params source -> IO ()
runAnalysisApp config = do
  tstore <- atomically $ newTVar mempty
  tident <- atomically $ newTVar 0
  s <- static "static"
  warpEnv $ GApp (app tstore tident) s
  where app tstore tident =
          App (analysisParser config)
              (analysisPrint config)
              (analysisFunc config)
              tident
              tstore
              (analysisTitle config)
              (analysisForm config)
              unCSV
        unCSV :: Monad m => CSVSettings -> Conduit source m ByteString
        unCSV = fromCSV
