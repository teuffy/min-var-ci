{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Foundation where

import           BasicPrelude
import           Yesod
import           Text.Lucius (luciusFile)

import           FP.ImportWizard.Temp (TempToken)

data App = App
    {   appApproot :: !Text }

instance Yesod App where
    defaultLayout w = do
        p <- widgetToPageContent $ do
            w
            toWidget $(luciusFile "templates/default-layout.lucius")
        mmsg <- getMessage
        giveUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p}
                    <link rel=stylesheet href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css">
                    ^{pageHead p}
                <body>
                    <div class="navbar navbar-inverse navbar-fixed-top">
                      <div class="navbar-inner">
                        <div class="container">
                          <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
                            <span class="icon-bar">
                            <span class="icon-bar">
                            <span class="icon-bar">
                          <a class="brand" href="http://fpcomplete.com/">
                            <img src="//fpcomplete.com/static/img/ide-logo.png" title="FP Complete">

                    <div class="container">
                        <h1>#{pageTitle p}
                        $maybe msg <- mmsg
                            <div class="alert alert-info alert-dismissable">
                                <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;
                                #{msg}
                        ^{pageBody p}
                |]
    approot = ApprootMaster appApproot

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "App" [parseRoutes|
/                       HomeR           GET
/add-source             AddSourceR      GET POST
/create-project         CreateProjectR  POST
/git/#TempToken/*Texts  GitR            GET
|]
