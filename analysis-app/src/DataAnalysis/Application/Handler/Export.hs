{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import Yesod

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Types

-- | Export the data source to various data formats.
getExportR :: Int -> String -> Handler Html
getExportR ident typ = do
    GApp app <- getYesod
    source <- getById app ident
    undefined
