{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | NOTE: This is included for testing of the repo. Generated code should not
-- include this module.

module Skel.Pharma.UserModel
    ( module Skel.Pharma.UserModel
    ) where
import Data.CSV.Conduit.Persist       (mkCsvPersist, persistCsv)

mkCsvPersist [persistCsv|
Dummy
   x Int
   y Int
   z Double
   deriving Show
|]
