-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Kmer.UserAnalysis (userAnalysis) where

import qualified Data.Text as T
import           Skel.Kmer.UserParameters
import qualified Data.ByteString as S

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.

-- For a list of commonly used functions to use in an analysis, see:
--
-- http://download.fpcomplete.com/tempdocs/data-analysis-library/DataAnalysis-Library.html
userAnalysis :: KmerParams -> Conduit S.ByteString Handler DataPoint
userAnalysis (KmerParams count) =
        startAnalysis

        -- Parse the incoming data as FASTA
    =$= parseFasta
    =$= conduitKmers
    =$= kmerHistogram
    =$= regroupKmers (T.take count)
    =$= kmerDataPoint
