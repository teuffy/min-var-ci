{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -Werror #-}

module FP.ImportWizard.Handler.Home where

import           FP.ImportWizard.Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Import wizard: Data sources"
    [whamlet|
        <p>You do not have any data sources
        <form method=get action=@{AddSourceR}>
            <input type="submit" value="Add a data source">
    |]

