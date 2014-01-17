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

module DataAnalysis.Application.Types.Stock where

import           DataAnalysis.Application.Types

import           Control.Applicative
import           Control.Lens
import           Data.ByteString     (ByteString)
import           Data.Conduit
import qualified Data.Map            as Map
import           Data.Text           (Text, unpack)
import           Data.Time           (Day)
import           Safe                (readMay)

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
