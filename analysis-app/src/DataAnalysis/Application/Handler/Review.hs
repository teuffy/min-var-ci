{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Review the imported data, and the analysis upon that data.

module DataAnalysis.Application.Handler.Review where

import           Data.Aeson
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import           Data.Time
import           DataAnalysis.Application.Foundation
import           System.Locale
import           Yesod
import           Yesod.Default.Util

import           DataAnalysis.Application.Types

-- | Review the imported data, and the analysis upon that data.
getReviewR :: Text -> Handler Html
getReviewR ident = do
    app <- getYesod
    source <- getById ident
    currentRoute <- getCurrentRoute
    let title = toHtml (formatTime defaultTimeLocale "Import %T" (srcTimestamp source))
    SomeAnalysis{..} <- return (appAnalysis app)
    ((result, widget), enctype) <- runFormPost (renderDivs analysisForm)
    start <- liftIO getCurrentTime
    let params =
          case result of
            FormSuccess p -> p
            _ -> def
    datapoints <- sourceFile (srcPath source) $= analysisConduit params $$ CL.consume
    now <- liftIO getCurrentTime
    defaultLayout $ do
        setTitle title
        let datapointsJson = toHtml (decodeUtf8 (encode (take 100 datapoints)))
            generationTime = diffUTCTime now start
        $(widgetFileReload def "review")

-- | For the parameters form.
postReviewR :: Text -> Handler Html
postReviewR = getReviewR
