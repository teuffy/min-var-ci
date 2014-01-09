{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Dispatch where

import Yesod

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Handler.Datasources
import DataAnalysis.Application.Handler.Help
import DataAnalysis.Application.Handler.Home
import DataAnalysis.Application.Handler.Import
import DataAnalysis.Application.Handler.Review

mkYesodDispatch "App" resourcesApp
