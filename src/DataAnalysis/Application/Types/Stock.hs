{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module DataAnalysis.Application.Types.Stock
    ( module DataAnalysis.Application.Types.Stock
    , module X
    ) where

import           Control.Applicative            as X
import           Control.Lens                   as X
import           Data.ByteString                (ByteString)
import           Data.CSV.Conduit.Persist       (mkCsvPersist, persistCsv)
import           Data.Conduit                   as X
import           Data.Conduit.Analysis          as X
import           Data.Conduit.List              as X (isolate)
import qualified Data.Conduit.List              as CL
import           Data.Default                   as X
import qualified Data.Map                       as Map
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, pack, unpack)
import           Data.Time                      (Day)
import           Data.Vector                    as X (Vector)
import           DataAnalysis.Application.Types as X
import           Safe                           (readMay)
import           Yesod                          as X hiding ((.=), (<.))

mkCsvPersist [persistCsv|
Stock
   date            Day "format=%F"
   open            Double
   high            Double
   low             Double
   close           Double
   volume          Int
   adjClose        Double
   deriving Show
|]