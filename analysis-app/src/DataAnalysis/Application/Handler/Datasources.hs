{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Datasources where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

getDatasourcesR :: Handler Html
getDatasourcesR = do
    GApp app <- getYesod
    sources <- getList app
    defaultLayout $ do
        setTitle "Datasources"
        $(widgetFileReload def "datasources")
