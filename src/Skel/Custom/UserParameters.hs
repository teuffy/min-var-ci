-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Custom.UserParameters (CustomParams (..), module X) where

import DataAnalysis.Application.Import as X

-- | Parameters to the analysis.
data CustomParams = CustomParams
    { cpDummy  :: !Int
    }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm CustomParams where
    form = CustomParams
        <$> areq intField "A parameter" (Just (cpDummy def))

-- | Default values for the parameters.
instance Default CustomParams where
  def = CustomParams 0
