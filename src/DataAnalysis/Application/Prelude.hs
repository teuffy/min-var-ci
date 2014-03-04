{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  ( runAnalysisApp
  , runAnalysisAppRaw
  , runAnalysisAppDb
  )
  where

import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit)
import Data.Text (Text)
import Data.Time
import DataAnalysis.Application.Dispatch ()
import DataAnalysis.Application.Types
import Database.Persist.Sqlite
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Yesod
import Yesod.Static

import Skel.MVP.UserModel

-- | Run the analysis web app.
runAnalysisApp :: (PersistEntity b,HasForm params)
               => Text
               -> (params -> Conduit b (ReaderT (FilterLog -> IO ()) (HandlerT App IO)) DataPoint)
               -> IO ()
runAnalysisApp title analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  pool <- makePool (return ())
  warpEnv
    (App man
         title
         (getSomeAnalysis analysis)
         s
         now
         pool)

-- | Run the analysis web app with a conduit from a raw 'ByteString'.
runAnalysisAppRaw :: HasForm params
                  => Text
                  -> (params -> Conduit ByteString (ReaderT (FilterLog -> IO ()) (HandlerT App IO)) DataPoint)
                  -> IO ()
runAnalysisAppRaw title analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  pool <- makePool (return ())
  warpEnv
    (App man
         title
         (getSomeAnalysisRaw analysis)
         s
         now
         pool)

-- | Run the analysis web app with a conduit from the database.
runAnalysisAppDb
  :: HasForm params =>
     Text
     -> SqlPersistT IO a
     -> (params
         -> Conduit Security (YesodDB App) DataPoint)
     -> IO ()
runAnalysisAppDb title migrate analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  pool <- makePool migrate
  warpEnv
    (App man
         title
         (getSomeAnalysisDb analysis)
         s
         now
         pool)

-- | Make a pool and migrate it.
makePool :: SqlPersistT IO a -> IO ConnectionPool
makePool migrate =
  do pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
     day <- fmap utctDay getCurrentTime
     runSqlPool (do migrate
                    insert (Security "YHOO" day 123)
                    insert (Security "GOOG" day 456))
                pool
     return pool
