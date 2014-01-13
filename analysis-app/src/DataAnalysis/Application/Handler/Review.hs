{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Review where

import Data.Aeson
import Data.Default
import Data.Text.Lazy.Encoding
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Types

getReviewR :: Int -> Handler Html
getReviewR ident = do
    GApp app <- getYesod
    source <- getById app ident
    currentRoute <- getCurrentRoute
    let title = toHtml (show (srcTimestamp source))
        printer = appPrinter app
    ((result, widget), enctype) <- runFormPost (appParamsForm app)
    datapoints <-
      liftIO (appAnalyzer
                app
                (case result of
                   FormSuccess p -> p
                   _ -> def)
                (srcParsed source))
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
            datapointsJson = toHtml (decodeUtf8 (encode datapoints))
        $(widgetFileReload def "review")

postReviewR :: Int -> Handler Html
postReviewR = getReviewR
