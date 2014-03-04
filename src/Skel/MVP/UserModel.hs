-- NOTE: This is included for testing of the repo. Generated code should not
-- include this module.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Skel.MVP.UserModel
    ( module Skel.MVP.UserModel
    ) where

import Data.CSV.Conduit.Persist
import           Database.Persist.TH

-- | Import format.
mkCsvPersist
  [persistCsv|
Stock invalidRows=stop format=csv
    date            Day "format=%F"
    open            Double
    high            Double
    low             Double
    close           Double
    volume          Int
    adjClose        Double
    deriving Show
|]

-- | Database format.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Security
  security Text
  date Day
  close Double
  deriving Show
|]

-- | Prices for two different securities on the same day.
data DatePrices =
  DatePrices Day Double Double
  deriving (Show)
