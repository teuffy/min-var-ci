{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields -fno-warn-orphans #-}

module FP.ImportWizard.Main where

import qualified Data.Text                    as Text
import           System.Environment           (getEnv)
import           System.Process               (system)

import           FP.ImportWizard.Handler.Home
import           FP.ImportWizard.Import

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
    _ <- system "rm -rf skel && mkdir skel && curl --silent https://s3.amazonaws.com/download.fpcomplete.com/data-analysis-demo/skel/v3.tgz | tar xzf - -C skel"
    root <- Text.pack <$> getEnv "APPROOT"
    warpEnv (App root)
