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
import Yesod.Static
import Data.Conduit (Conduit)

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

#if MIN_VERSION_conduit(2, 0, 0)
import Network.HTTP.Client (defaultManagerSettings, newManager)
#else
import Network.HTTP.Conduit (Manager, newManager, def)
defaultManagerSettings = def
#endif

-- | Run the analysis web app.
runAnalysisApp :: (FromMapRow input, HasForm params)
               => Text
               -> (params -> Conduit input (HandlerT App IO) DataPoint)
               -> IO ()
runAnalysisApp title analysis = do
  s <- static "static"
  man <- newManager defaultManagerSettings
  warpEnv
    (App man
         title
         (getSomeAnalysis analysis)
         s)
