-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Pharma.UserAnalysis (userAnalysis) where

import qualified Data.Conduit.List as CL
import           Data.Semigroup
import           Data.Text (pack)

import           Skel.Pharma.UserModel
import           Skel.Pharma.UserParameters

-- The following is an implementation of relative strength index.
-- For more information, see: http://en.wikipedia.org/wiki/Relative_strength_index

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.
userAnalysis :: MonadIO m => PharmaParams -> Conduit Dummy m DataPoint
userAnalysis (PharmaParams _min _max) =
  CL.map (\(Dummy x y z) -> DP3 (D3D x y z))
