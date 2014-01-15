{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module DataAnalysis.Application.Types.Stock where

import Data.Conduit
import Yesod
import           Control.Lens
import qualified Data.Conduit.Binary   as CB
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Text     as CT
import           Data.CSV.Conduit
import           Data.Time             (Day)
import qualified Data.Conduit.Analysis as CA
import qualified Data.Vector as V
import Network.HTTP.Conduit hiding (Proxy)
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (Builder, fromByteString)
import Control.Exception (Exception)
import Control.Applicative
import Data.Text (Text, unpack, pack)
import Data.Typeable (Typeable)
import Safe (readMay)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Demo.Helper.Class
import Data.Proxy
import Demo.Common

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

data UpDown = UpDown
    { _udDay  :: !Day
    , _udUp   :: !Double
    , _udDown :: !Double
    }
    deriving Show
makeClassy ''UpDown

stocksToUpDown :: Monad m => Conduit Stock m UpDown
stocksToUpDown =
    await >>= maybe (return ()) loop
  where
    loop today = do
        myesterday <- await
        case myesterday of
            Nothing -> return ()
            Just yesterday -> do
                let ud = UpDown
                        { _udDay = today ^. stockDay
                        , _udUp = max 0 $ (today ^. stockAdjClose) - (yesterday ^. stockAdjClose)
                        , _udDown = max 0 $ (yesterday ^. stockAdjClose) - (today ^. stockAdjClose)
                        }
                yield ud
                loop yesterday

rawStockSource :: (MonadResource m, MonadBaseControl IO m, ManagerReader m)
               => Text -- ^ stock symbol
               -> Source m ByteString
rawStockSource symbol = sourceURL $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ unpack symbol
