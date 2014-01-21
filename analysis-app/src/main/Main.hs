{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | Main entry point to my analysis app.

module Main where

import           DataAnalysis.Application.Prelude
import           DataAnalysis.Application.Types
import           DataAnalysis.Application.Types.Stock

import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.Analysis                as CA
import qualified Data.Conduit.List                    as CL
import           Data.Default
import           Data.Proxy
import           Data.Text                            (pack)
import qualified Data.Vector                          as V
import           UserAnalysis
import           UserParameters
import           Yesod

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "RSI analysis" userAnalysis
