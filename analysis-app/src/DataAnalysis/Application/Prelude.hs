{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (runAnalysisApp)
  where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Yesod
import Data.Time
import Yesod.Static

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

#ifdef FPHC
import Network.HTTP.Conduit (Manager, newManager, def)
defaultManagerSettings = def
#else
import Network.HTTP.Client (defaultManagerSettings, newManager)
#endif

-- | Run the analysis web app.
runAnalysisApp :: HasAnalysis params => Text -> Proxy params -> IO ()
runAnalysisApp title params = do
  s <- static "static"
  man <- newManager defaultManagerSettings
  now <- getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysis params)
         s
         now)
