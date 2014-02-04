-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Kmer.UserParameters
    ( module Skel.Kmer.UserParameters
    , module DataAnalysis.Application.Import
    ) where

import DataAnalysis.Application.Import

-- | Parameters to the analysis.
data KmerParams = KmerParams
    { kpKmerSize :: !Int -- ^ Number of bases in a kmer, should be 1-4
    }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm KmerParams where
    form = KmerParams
        <$> areq intField "Number of bases in a k-mer, should be 1-4" (Just (kpKmerSize def))

-- | Default values for the parameters.
instance Default KmerParams where
  def = KmerParams 2
