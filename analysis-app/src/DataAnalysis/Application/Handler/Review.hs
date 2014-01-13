{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Review the imported data, and the analysis upon that data.

module DataAnalysis.Application.Handler.Review where

import Data.Aeson
import Data.Default
import Data.Text.Lazy.Encoding
import Data.Time
import System.Locale
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Types

-- | Review the imported data, and the analysis upon that data.
getReviewR :: Int -> Handler Html
getReviewR ident = do
    GApp app <- getYesod
    source <- getById app ident
    currentRoute <- getCurrentRoute
    let title = toHtml (formatTime defaultTimeLocale "Import %T" (srcTimestamp source))
        printer = appPrinter app
    ((result, widget), enctype) <- runFormPost (appParamsForm app)
    start <- liftIO getCurrentTime
    !datapoints <-
      liftIO (appAnalyzer
                app
                (case result of
                   FormSuccess p -> p
                   _ -> def)
                (srcParsed source))
    now <- liftIO getCurrentTime
    defaultLayout $ do
        setTitle title
        let sample =
              [whamlet|
                <ul>
                  $forall row <- take 20 (srcParsed source)
                    <li>
                      <code>
                       #{toHtml (printer row)}|]
            output =
              [whamlet|
                <ul>
                  $forall datapoint <- take 20 datapoints
                    <li>
                      <code>
                       #{toHtml (show datapoint)}|]
            datapointsJson = toHtml (decodeUtf8 (encode (take 100 datapoints)))
            generationTime = diffUTCTime now start
        $(widgetFileReload def "review")

-- | For the parameters form.
postReviewR :: Int -> Handler Html
postReviewR = getReviewR
