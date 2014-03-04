{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Review the imported data, and the analysis upon that data.

module DataAnalysis.Application.Handler.Review where

import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Lens
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.IORef
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding
import           Data.Time
import           DataAnalysis.Application.Foundation
import           System.Locale
import           Yesod
import           Yesod.Default.Util

import           DataAnalysis.Application.Analyze
import           DataAnalysis.Application.Types

-- | Reload the data source.
getReloadR :: Text -> Int -> Handler TypedContent
getReloadR ident i = do
  (_,changed) <- getById ident (Just (fromIntegral i))
  respondSource "text/plain"
                (CL.sourceList [changed] $= CL.map (Chunk . fromShow))

-- | Review the imported data, and the analysis upon that data.
getReviewR :: Text -> Handler Html
getReviewR ident = do
  SomeAnalysis{..} <- fmap appAnalysis getYesod
  (widget,enctype) <- runFormWithPolling (makeParamsForm analysisForm)
  (source,_) <- getById ident Nothing
  (datapoints,rows,timing) <- runBenchedAnalysis ident
  defaultLayout $ do
    let title = toHtml (formatTime defaultTimeLocale "Import %T" (srcTimestamp source))
        datapointsJson = toHtml (decodeUtf8 (encode (take 100 datapoints)))
        messages = toListOf (traverse . _DPM) datapoints
        murl = srcUrl source
    setTitle title
    $(widgetFileReload def "review")

-- | Run the polling form, this is only activated from JavaScript.
runFormWithPolling urlForm =
  do ((r,widget),enctype) <- runFormGet urlForm
     return
       (widget
       ,enctype)

-- | Run the analysis of the given data source, counting rows
-- processed and timing the process.
runBenchedAnalysis :: Text -> Handler ([DataPoint],Int,NominalDiffTime)
runBenchedAnalysis ident =
  (do countRef <- liftIO (newIORef 0)
      logRef <- liftIO (newIORef id)
      start <- liftIO getCurrentTime
      source <- analysisSource ident countRef logRef
      !datapoints <- runDB (source $$ CL.consume)
      rows :: Int <- liftIO (readIORef countRef)
      logs <- fmap ($ []) $ liftIO $ readIORef logRef
      now <- liftIO getCurrentTime
      let timing = diffUTCTime now start
      return (datapoints ++ mapMaybe logToDP logs,rows,timing))
  where
    logToDP (FilterLog idx msg sfa) =
        fmap DPM $ case sfa of
            SFAKeep -> Nothing
            SFADrop -> Just $ "Dropped record #" <> pack (show idx) <> msg'
            SFAReplace -> Just $ "Modified record #" <> pack (show idx) <> msg'
      where
        msg' = if T.null msg then "" else ": " <> msg

-- | Show a number that's counting something so 1234 is 1,234.
showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

-- | Review the imported data, and the analysis upon that data.
postReviewR :: Text -> Handler Html
postReviewR = getReviewR
