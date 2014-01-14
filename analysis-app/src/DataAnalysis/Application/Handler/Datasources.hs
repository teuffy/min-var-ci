{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Datasources where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Types

getDatasourcesR :: Handler Html
getDatasourcesR = do
    GApp app _ <- getYesod
    sources <- getList app
    defaultLayout $ do
        setTitle "Datasources"
        $(widgetFileReload def "datasources")
