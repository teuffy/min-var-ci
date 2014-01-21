{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (runAnalysisApp)
  where

import Data.Text (Text)
import Yesod
import Data.Time
import Yesod.Static
import Data.Conduit (Conduit)

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

#if FPHC
import Network.HTTP.Conduit (Manager, newManager, def)
defaultManagerSettings = def
#else
import Network.HTTP.Client (defaultManagerSettings, newManager)
#endif

-- | Run the analysis web app.
runAnalysisApp :: (FromMapRow input, HasForm params)
               => Text
               -> (params -> Conduit input (HandlerT App IO) DataPoint)
               -> IO ()
runAnalysisApp title analysis = do
  s <- static "static"
  man <- newManager defaultManagerSettings
  now <- getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysis analysis)
         s
         now)
