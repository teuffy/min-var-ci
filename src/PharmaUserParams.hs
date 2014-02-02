-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module PharmaUserParams (PharmaParams (..), module X) where

import DataAnalysis.Application.PharmaImport as X

-- | Parameters to the analysis.
data PharmaParams = PharmaParams
    { ppMax :: Double
    , ppMin :: Double
    }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm PharmaParams where
    form = PharmaParams
        <$> areq doubleField "Min height" (Just (ppMin def))
        <*> areq doubleField "Max height" (Just (ppMax def))

-- | Default values for the parameters.
instance Default PharmaParams where
  def = PharmaParams 0
                     10000
