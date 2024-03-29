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
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Conduit

import Data.IORef
import Data.Text (Text)
-- import DataAnalysis.Application.Foundation
import Yesod
-- --
import DataAnalysis.Application.Types

-- | Analyze the imported data with the submitted parameters (if any),
-- and return the data points from it.
analysisSource :: IORef Int
               -> IORef ([a] -> c)
               -> HandlerT App IO (ConduitM () DataPoint (YesodDB App) (),[Text])
analysisSource countRef logRef = do
    app <- getYesod
    {-(source,_) <- getById ident Nothing-}
    SomeAnalysis{..} <- return (appAnalysis app)
    ((result, _), _) <- runFormGet (makeParamsForm analysisForm)
    let params =
          case result of
            FormSuccess (p,_::Text,_,_) -> p
            _ -> analysisDefaultParams
        logger fl = modifyIORef logRef (. (fl:))
    return (case analysisConduit of
              Left dbconduit ->
                (dbconduit countRef params,(case result of
                                              FormFailure xs -> xs
                                              _ -> [])))

runReaderC :: Monad m => env -> Conduit i (ReaderT env m) o -> Conduit i m o
runReaderC env = transPipe (`runReaderT` env)

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
