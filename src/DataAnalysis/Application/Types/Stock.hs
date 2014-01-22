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

module DataAnalysis.Application.Types.Stock
    ( module DataAnalysis.Application.Types.Stock
    , module X
    ) where

import Control.Applicative            as X
import Control.Lens                   as X
import Data.CSV.Conduit.Persist       (mkCsvPersist, persistCsv)
import Data.Conduit                   as X
import Data.Conduit.Analysis          as X
import Data.Conduit.List              as X (isolate)
import Data.Default                   as X
import Data.Time                      (Day)
import Data.Vector                    as X (Vector)
import DataAnalysis.Application.Types as X
import Yesod                          as X hiding ((.=), (<.))

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
