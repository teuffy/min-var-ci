{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Review the imported data, and the analysis upon that data.

module DataAnalysis.Application.Handler.Review where

import           Control.Lens
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.IORef
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import           Data.Time
import           DataAnalysis.Application.Foundation
import           System.Locale
import           Yesod
import           Yesod.Default.Util

import           DataAnalysis.Application.Analyze
import           DataAnalysis.Application.Types

-- | Review the imported data, and the analysis upon that data.
getReviewR :: Text -> Handler Html
getReviewR ident = do
  SomeAnalysis{..} <- fmap appAnalysis getYesod
  (widget,mpoll,enctype) <- runFormWithPolling (makeParamsForm analysisForm)
  source <- getById ident mpoll
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
       ,case r of
          FormSuccess (_,_,poll,interval) ->
            if poll
               then Just (fromInteger interval)
               else Nothing
          _ -> Nothing
       ,enctype)

-- | Run the analysis of the given data source, counting rows
-- processed and timing the process.
runBenchedAnalysis :: Text -> Handler ([DataPoint],Int,NominalDiffTime)
runBenchedAnalysis ident =
  (do countRef <- liftIO (newIORef 0)
      start <- liftIO getCurrentTime
      !datapoints <- analysisSource ident countRef >>= ($$ CL.consume)
      rows :: Int <- liftIO (readIORef countRef)
      now <- liftIO getCurrentTime
      let timing = diffUTCTime now start
      return (datapoints,rows,timing))

-- | Show a number that's counting something so 1234 is 1,234.
showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

-- | Review the imported data, and the analysis upon that data.
postReviewR :: Text -> Handler Html
postReviewR = getReviewR
