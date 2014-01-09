{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example analysis.

module Analysis where

import Data.Function
import Data.List
import Data.Ord
import DataAnalysis.Application.Types
import Types

-- | Some analysis function.
analysis :: Parameters -> [Listen] -> IO [DataPoint]
analysis Parameters{..} =
  return .
  map datapoint .
  groupBy (on (==) listenArtist) .
  sortBy (comparing listenArtist)
  where datapoint (Listen _ artist _:xs) = Tuple artist (fromIntegral (1 + length xs))
        datapoint [] = Tuple "misc" 0
