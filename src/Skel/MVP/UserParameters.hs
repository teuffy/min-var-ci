-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module Skel.MVP.UserParameters
    ( module Skel.MVP.UserParameters
    , module Skel.MVP.UserModel
    , module DataAnalysis.Application.Import
    ) where

import Skel.MVP.UserModel
import DataAnalysis.Application.Import

-- | Parameters to the analysis.
data MvpParams = MvpParams
    -- { rpSize  :: !Int -- ^ MVP grouping size
    -- , rpAlpha :: !Double -- ^ alpha for exponential moving average
    -- , rpBars  :: !Int -- ^ number of bars to display
    -- }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm MvpParams where
    form = pure MvpParams
        -- <$> areq intField "Grouping size" (Just (rpSize def))
        -- <*> areq doubleField "Alpha (for exponential moving average)" (Just (rpAlpha def))
        -- <*> areq intField "Number of bars (up to 20 for a readable graph)" (Just (rpBars def))

-- | Default values for the parameters.
instance Default MvpParams where
  def = MvpParams
