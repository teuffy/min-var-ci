-- NOTE: This is included for testing of the repo. Generated code should not
-- include this module.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Skel.Custom.UserModel
    ( module Skel.Custom.UserModel
    ) where
import Data.CSV.Conduit.Persist

mkCsvPersist
  [persistCsv|
CustomData invalidRows=stop format=csv
    dummy           Int
    deriving Show
|]