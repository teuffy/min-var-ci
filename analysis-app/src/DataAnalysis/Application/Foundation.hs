{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module DataAnalysis.Application.Foundation where

import           Control.Concurrent.STM
import           Data.ByteString.Lazy           (ByteString)
import           Data.Default
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import           Data.Text                      (Text)
import           Data.Time
import           DataAnalysis.Application.Types
import           Text.Blaze
import           Text.Hamlet
import           Yesod
import           Yesod.Default.Util

data App source params = App
  { appParser    :: !(ByteString -> IO (Maybe [source]))
  , appPrinter   :: !(source -> Text)
  , appAnalyzer  :: !(params -> [source] -> IO [DataPoint])
  , appCounter   :: !(TVar Int)
  , appStore     :: !(TVar (IntMap (Source source)))
  , appTitle     :: !Text
  , appDefParams :: params
  }

data Source source = Source
  { srcParsed    :: ![source]
  , srcTimestamp :: !UTCTime
  }

data GenericApp = forall source params. GApp (App source params)

mkYesodData "GenericApp" $(parseRoutesFile "config/routes")

instance ToMarkup (Route GenericApp) where
  toMarkup r =
    case r of
      HomeR        -> "Home"
      ReviewR _    -> "Review"
      HelpR        -> "Help"
      ImportR      -> "Import"
      DatasourcesR -> "Data Sources"

instance Yesod GenericApp where
  defaultLayout widget = do
    pc <- widgetToPageContent $(widgetFileNoReload def "default-layout")
    currentRoute <- getCurrentRoute
    yesod <- getYesod
    case yesod of
      GApp (App{appTitle=reviewTitle}) ->
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage GenericApp FormMessage where
  renderMessage _ _ = defaultFormMessage

-- | Next the next unique ID.
getNextId :: App source params -> STM Int
getNextId (App{appCounter=tnextId}) = do
    nextId <- readTVar tnextId
    writeTVar tnextId $ nextId + 1
    return nextId

-- | Get all sources.
getList :: App source params -> Handler [(Int, Source source)]
getList App{appStore=tstore} = do
    store <- liftIO $ readTVarIO tstore
    return $ IntMap.toList store

-- | Add a new source.
addSource :: App source params -> [source] -> Handler Int
addSource app@(App{appStore=tstore}) parsed = do
    now <- liftIO getCurrentTime
    liftIO . atomically $ do
        ident <- getNextId app
        modifyTVar tstore
                   (IntMap.insert ident
                                  (Source parsed now))
        return ident

-- | Get a source by its id.
getById :: App source params -> Int -> Handler (Source source)
getById App{appStore=tstore} ident = do
    store <- liftIO $ readTVarIO tstore
    case IntMap.lookup ident store of
      Nothing -> notFound
      Just source -> return source
