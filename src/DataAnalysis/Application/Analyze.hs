{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Functionality to perform an analysis on the imported data source,
-- the submitted paramaters and return some data points.

module DataAnalysis.Application.Analyze where

import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary (sourceFile)
import Data.IORef
import Data.Text (Text)
import DataAnalysis.Application.Foundation
import Yesod

import DataAnalysis.Application.Types

-- | Analyze the imported data with the submitted parameters (if any),
-- and return the data points from it.
analysisSource :: Text -> IORef Int -> HandlerT App IO (Source Handler DataPoint)
analysisSource ident countRef = do
    app <- getYesod
    source <- getById ident Nothing
    SomeAnalysis{..} <- return (appAnalysis app)
    ((result, _), _) <- runFormGet (makeParamsForm analysisForm)
    let params =
          case result of
            FormSuccess (p,_::Text,_,_) -> p
            _ -> analysisDefaultParams
    return (sourceFile (srcPath source) $= analysisConduit countRef params)

-- | Make the parameters form, includes some hidden fields used by JavaScript magic.
makeParamsForm analysisForm =
  renderDivs ((,,,) <$> analysisForm <*> graphType <*> doPoll <*> pollInterval)
  where graphType =
          areq hiddenField
               "" {fsName = l,fsId = l}
               (Just ("Bar" :: Text))
          where l = Just "graph_type"
        doPoll =
          areq checkBoxField
               "Poll" {fsName = l,fsId = l}
               (Just False)
          where l = Just "poll"
        pollInterval =
          areq intField
               "Interval" {fsName = l,fsId = l}
               (Just (3::Integer))
          where l = Just "interval"
