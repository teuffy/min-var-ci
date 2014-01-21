{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields -fno-warn-orphans #-}

module FP.ImportWizard.Main where

import qualified Data.Text                    as Text
import           System.Environment           (getEnv)

import           FP.ImportWizard.Handler.Home
import           FP.ImportWizard.Import

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
    root <- Text.pack <$> getEnv "APPROOT"
    warpEnv (App root)