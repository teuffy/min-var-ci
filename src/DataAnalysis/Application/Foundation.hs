{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS -fno-warn-orphans #-}

module DataAnalysis.Application.Foundation where

import           Control.Exception              (IOException)
import qualified Control.Exception              as E
import           Control.Monad
import           Data.Conduit
import           Data.Conduit.Binary            (sinkFile, sourceFile)
import           Data.Conduit.Equal
import           Data.Conduit.Zlib
import           Data.Conduit.Zlib.Auto
import           Data.Default
import           Data.List
import           Data.Text                      (Text, pack)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           DataAnalysis.Application.Types
import           Network.HTTP.Client
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Time
import           Text.Blaze
import           Text.Hamlet
import           Yesod
import           Yesod.Core.Types
import           Yesod.Default.Util
import           Yesod.Static

mkYesodData "App" $(parseRoutesFile "config/routes")

instance HasManager App where
    manager = appManager

instance ToMarkup (Route App) where
  toMarkup r =
    case r of
      HomeR        -> "Home"
      ReviewR _    -> "Review"
      HelpR        -> "Help"
      ImportR      -> "Import"
      DatasourcesR -> "Data Sources"
      ExportR {}   -> "Export"
      StaticR {}   -> "Static"
      StartTimeR{} -> "Start time"
      ReloadR{}    -> "Reload"

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ do
      $(widgetFileNoReload def "default-layout")
    currentRoute <- getCurrentRoute
    yesod <- getYesod
    case yesod of
      App{appTitle}  ->
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- | Add a new source.
--
-- TODO: Generate a proper temporary filename.
--
-- TODO: Currently this just moves the uploaded file to another place,
-- leaving it in CSV format. That means when we're doing analysis,
-- we're always re-parsing, in chunks the CSV from file. While that's
-- nice and good for constant memory usage, a CSV is possibly not the
-- fastest format in the long-term (or maybe it is not a bottle
-- neck?). We can probably use something like cereal/binary as
-- conduits, like here:
-- https://hackage.haskell.org/package/cereal-conduit-0.7.2/docs/Data-Conduit-Cereal.html
--
-- But in the meantime, for the demo, this should be fine. Though it
-- looks pretty easy to use the cereal conduit laid out there.
--
-- If/when we decide we're going to read from an alternate data form,
-- this function should use the fileSource function to make a Producer
-- to write into some other format, and then store whatever ID is used
-- for that format (file, DB, redis, w/e), and the conduit source
-- changed from sourceFile to whatever else.
addSource :: FileInfo -> Handler Text
addSource fi = do
  fp <- liftIO getTemporaryFileName
  let name = pack (takeFileName fp)
  if T.isSuffixOf ".gz" (fileName fi)
     then do liftIO
               (runResourceT
                  (fileSourceRaw fi $= ungzip $$ sinkFile fp))
             return name
     else do liftIO (fileMove fi fp)
             return name

-- | Import a source from a URL.
addUrlSource :: Text -> Handler (Maybe Text)
addUrlSource url = do
  fp <- liftIO getTemporaryFileName
  addUrlSourceWithFP url fp

-- | Get a temporary filename for an upload.
getTemporaryFileName :: IO FilePath
getTemporaryFileName =
  do appDir <- getAppDir
     createDirectoryIfMissing True appDir
     (fp,h) <- openTempFile appDir "upload.csv"
     hClose h
     return fp

-- | Get a source by its id. Returns whether the source has changed.
getById :: Text -> Maybe NominalDiffTime -> Handler (DataSource,Bool)
getById ident mpoll = do
  files <- getList
  case find ((==ident).srcName) files of
    Nothing -> error "No such imported data source."
    Just s ->
      do case mpoll of
           Nothing -> return (s,False)
           Just poll ->
             do now <- liftIO getCurrentTime
                if addUTCTime poll (srcTimestamp s) < now
                   then updateSource s
                   else return (s,False)

-- | Update the given data source by its URL. Returns whether the
-- source has changed.
updateSource :: DataSource -> Handler (DataSource,Bool)
updateSource s =
  case srcUrl s of
    Nothing ->
      return (s,False)
    Just url ->
      do let fp = srcPath s
             oldfp = srcPath s ++ ".old"
         liftIO (renameFile fp oldfp)
         _ <- addUrlSourceWithFP (T.pack url) fp
         equal <- sourcesEqual (sourceFile fp $= autoUngzip)
                               (sourceFile oldfp)
         return (s,maybe False not equal)

-- | Add a source downloaded from a URL.
addUrlSourceWithFP :: Text -> FilePath -> Handler (Maybe Text)
addUrlSourceWithFP url fp = do
  mgr <- fmap appManager getYesod
  request <- parseUrl (T.unpack url)
  response <- http request mgr
  case statusCode (responseStatus response) of
    200 ->
      do responseBody response $$+-
           (autoUngzip =$ sinkFile fp)
         liftIO (T.writeFile (fp ++ ".url") url)
         return (Just (pack (takeFileName fp)))
    _ ->
      return Nothing

-- | Get all sources.
getList :: Handler [DataSource]
getList =
  fmap (sortBy (flip compare)) (liftIO getItems)
  where
    getItems =
      do dir <- getAppDir
         list <- fmap (filter isSource)
                      (E.catch (getDirectoryContents dir)
                               (\(_::E.IOException) -> return []))
         forM list (makeDataSource dir)
    isSource fp = not (all (=='.') fp) && extensionIs fp "csv"
    extensionIs fp ext = takeWhile (/='.') (reverse fp) == reverse ext

-- | Make a data source.
makeDataSource :: String -> String -> IO DataSource
makeDataSource dir item =
  do let fp = dir </> item
     time <- fmap utcTimeFromClockTime
                  (getModificationTime fp)
     murl <- fmap (either (const Nothing :: IOException -> Maybe a) Just)
                  (E.try (readFile (fp ++ ".url")))
     return (DataSource (pack item) fp time murl)

-- | Get the directory used for uploads.
getAppDir :: IO FilePath
getAppDir =
  do tmp <- getTemporaryDirectory
     let appDir = tmp ++ "/analysis-app"
     return appDir

-- | An unfortunate conversion function that is necessary in this
-- version of the directory package.
utcTimeFromClockTime :: ClockTime -> UTCTime
utcTimeFromClockTime (TOD seconds picoseconds) =
  posixSecondsToUTCTime
    (fromRational
       (fromInteger seconds + fromInteger picoseconds / 1000000000000))
