{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (runAnalysisApp)
  where

import Control.Concurrent.STM
import Data.Monoid
import Data.Proxy (Proxy)
import Data.Text (Text)
import Yesod
import Yesod.Static

#ifdef FPHC
import Network.HTTP.Conduit (Manager, newManager, def)
defaultManagerSettings = def
#else
import Network.HTTP.Client (defaultManagerSettings, newManager)
#endif

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

-- | Run the analysis web app.
runAnalysisApp :: HasAnalysis params => Text -> Proxy params -> IO ()
runAnalysisApp title params = do
  counter <- atomically (newTVar 0)
  store <- atomically (newTVar mempty)
  s <- static "static"
  man <- newManager defaultManagerSettings
  warpEnv
    (App man
         title
         (getSomeAnalysis params)
         s
         counter
         store)
