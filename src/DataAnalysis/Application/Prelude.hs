{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  ( runAnalysisApp
  , runAnalysisAppRaw
  )
  where

import Data.Conduit (Conduit)
import Data.Text (Text)
import Data.Time
import Yesod
import Yesod.Static

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()
import Data.ByteString (ByteString)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Run the analysis web app.
runAnalysisApp :: (PersistEntity b,HasForm params)
               => Text
               -> (params -> Conduit b (HandlerT App IO) DataPoint)
               -> IO ()
runAnalysisApp title analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysis analysis)
         s
         now)

-- | Run the analysis web app.
runAnalysisAppRaw :: HasForm params
                  => Text
                  -> (params -> Conduit ByteString (HandlerT App IO) DataPoint)
                  -> IO ()
runAnalysisAppRaw title analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysisRaw analysis)
         s
         now)
