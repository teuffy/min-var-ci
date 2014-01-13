{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example analysis.

module Analysis where

import Control.Lens
import Data.Function
import Data.List
import Data.Ord
import DataAnalysis.Application.Types
import Types

-- | Some analysis function.
analysis :: Parameters -> [Listen] -> IO [DataPoint]
analysis Parameters{..} =
  return .
  filter (\d -> maybe True (view dataValue d >=) paramStart &&
                maybe True (view dataValue d <=) paramEnd) .
  sortBy (flip (comparing (view dataValue))) .
  map datapoint .
  groupBy (on (==) listenArtist) .
  sortBy (comparing listenArtist)
  where datapoint (Listen _ _ artist:xs) =
          DP artist (fromIntegral (1 + length xs)) Nothing
        datapoint [] =
          DP "misc" 0 Nothing
