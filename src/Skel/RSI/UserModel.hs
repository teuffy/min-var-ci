-- NOTE: This is included for testing of the repo. Generated code should not
-- include this module.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Skel.RSI.UserModel
    ( module Skel.RSI.UserModel
    ) where
import Data.CSV.Conduit.Persist

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