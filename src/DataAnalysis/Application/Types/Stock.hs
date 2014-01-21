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
import           Data.Conduit                   as X
import           Data.Conduit.Analysis          as X
import           Data.Conduit.List              as X (isolate)
import qualified Data.Conduit.List              as CL
import           Data.Default                   as X
import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack, unpack)
import           Data.Time                      (Day)
import           Data.Vector                    as X (Vector)
import           DataAnalysis.Application.Types as X
import           Safe                           (readMay)
import           Yesod                          as X hiding ((.=), (<.))

data Stock = Stock
    { _stockDay      :: !Day
    , _stockOpen     :: !Double
    , _stockHigh     :: !Double
    , _stockLow      :: !Double
    , _stockClose    :: !Double
    , _stockVolume   :: !Double
    , _stockAdjClose :: !Double
    }
    deriving Show
makeClassy ''Stock

instance FromMapRow Stock where
    fromMapRow m = Stock
        <$> readColumn "Date"
        <*> readColumn "Open"
        <*> readColumn "High"
        <*> readColumn "Low"
        <*> readColumn "Close"
        <*> readColumn "Volume"
        <*> readColumn "Adj Close"
      where
        readColumn :: Read a => Text -> Either ParseError a
        readColumn k =
            case Map.lookup k m of
                Nothing -> Left $ ColumnNotFound k
                Just v ->
                    case readMay $ unpack v of
                        Nothing -> Left $ CouldNotReadColumn v
                        Just x -> Right x

instance PersistEntity Stock
