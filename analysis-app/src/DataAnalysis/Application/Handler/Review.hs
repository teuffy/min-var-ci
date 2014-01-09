{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Review where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

getReviewR :: Int -> Handler Html
getReviewR ident = do
    GApp app <- getYesod
    source <- getById app ident
    currentRoute <- getCurrentRoute
    let title = toHtml (show (srcTimestamp source))
        printer = appPrinter app
    datapoints <-
      liftIO (appAnalyzer
                app
                (appDefParams app)
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
        $(widgetFileNoReload def "review")
