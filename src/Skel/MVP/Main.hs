{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to an MVP analysis.

module Skel.MVP.Main where

import DataAnalysis.Application.Prelude
import Skel.MVP.UserAnalysis
import Skel.MVP.UserModel

import Database.Persist.Sqlite
import Control.Monad.Logger

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisAppDb "MVP analysis"
                        (runMigration migrateAll)
                        userAnalysis
