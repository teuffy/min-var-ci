{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Datasets where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

getDatasetsR :: Handler Html
getDatasetsR = do
    sources <- getList
    defaultLayout $ do
        setTitle "Datasets"
        $(widgetFileReload def "datasets")
