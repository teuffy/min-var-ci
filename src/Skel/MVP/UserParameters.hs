-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module Skel.MVP.UserParameters
    ( module Skel.MVP.UserParameters
    , module Skel.MVP.UserModel
    , module DataAnalysis.Application.Import
    , MvpParams(..)
    ) where

import qualified Data.Text as T
import           DataAnalysis.Application.Import
import           Skel.MVP.UserModel

-- | Parameters to the analysis.
data MvpParams = MvpParams
  { paramsFrom :: Maybe Day
  , paramsTo :: Maybe Day
  }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm MvpParams where
    form = MvpParams <$> date "From" <*> date "To"
      where date label =
              fmap Just
                   (areq (checkMMap (return . parseDate . T.unpack)
                                    (T.pack . show)
                                    textField)
                         label
                         Nothing)
