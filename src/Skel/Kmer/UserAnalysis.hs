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

        -- Convert the stream of individual nucleobases into a stream of k-mers
    =$= conduitKmers

        -- Count the frequency of each k-mer
    =$= kmerHistogram

        -- Perform a regrouping of the k-mers based on the
        -- given textual conversion.
    =$= regroupKmers (T.take count)

        -- Output each k-mer frequency as a datapoint for graphing.
    =$= kmerDataPoint
