{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module DataAnalysis.Application.Foundation where

import           DataAnalysis.Application.Types

import           Control.Concurrent.STM
import           Data.ByteString.Lazy           (ByteString)
import           Data.Default
import           Data.Dynamic
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import           Data.Text                      (Text)
import           Data.Time
import           Text.Blaze
import           Text.Hamlet
import           Yesod
import           Yesod.Default.Util

data App = App
  { appParser   :: !(ByteString -> IO (Maybe [Dynamic]))
  , appPrinter  :: !(Dynamic -> Text)
  , appAnalyzer :: !(Dynamic -> [Dynamic] -> IO [DataPoint])
  , appCounter  :: !(TVar Int)
  , appStore    :: !(TVar (IntMap Source))
  , appTitle    :: !Text
  }

data Source = Source
  { srcParsed    :: ![Dynamic]
  , srcTimestamp :: !UTCTime
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance ToMarkup (Route App) where
  toMarkup r =
    case r of
      HomeR        -> "Home"
      ReviewR _    -> "Review"
      HelpR        -> "Help"
      ImportR      -> "Import"
      DatasourcesR -> "Data Sources"

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $(widgetFileNoReload def "default-layout")
    currentRoute <- getCurrentRoute
    reviewTitle <- fmap appTitle getYesod
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- | Next the next unique ID.
getNextId :: App -> STM Int
getNextId (App{appCounter=tnextId}) = do
    nextId <- readTVar tnextId
    writeTVar tnextId $ nextId + 1
    return nextId

-- | Get all sources.
getList :: Handler [(Int, Source)]
getList = do
    App{appStore=tstore} <- getYesod
    store <- liftIO $ readTVarIO tstore
    return $ IntMap.toList store

-- | Add a new source.
addSource :: App -> [Dynamic] -> Handler Int
addSource app@(App{appStore=tstore}) parsed = do
    now <- liftIO getCurrentTime
    liftIO . atomically $ do
        ident <- getNextId app
        modifyTVar tstore
                   (IntMap.insert ident
                                  (Source parsed now))
        return ident

-- | Get a source by its id.
getById :: Int -> Handler Source
getById ident = do
    App{appStore=tstore} <- getYesod
    store <- liftIO $ readTVarIO tstore
    case IntMap.lookup ident store of
      Nothing -> notFound
      Just source -> return source
