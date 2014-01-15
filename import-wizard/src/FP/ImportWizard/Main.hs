{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -Wall -Werror -fno-warn-orphans #-}

module FP.ImportWizard.Main where

import           FP.ImportWizard.Handler.AddSource
import           FP.ImportWizard.Handler.Home
import           FP.ImportWizard.Import

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = warpEnv App
