{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Parser for the analyzed data type. This file is a temporary workaround.

module Parser where

import           Types

import           Control.Applicative
import qualified Data.ByteString as S (ByteString)
import           Data.ByteString.Lazy (ByteString)
import           Data.CSV.Conduit
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List    as C
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Time
import           System.Locale

-- | Read a CSV file from a byte string.
parseCSVSource
  :: CSVSettings
  -- ^ Settings to use in deciphering stream
  -> ByteString
  -- ^ Input string
  -> IO [Listen]
parseCSVSource set lbs =
  runResourceT $ sourceLbs lbs $= intoCSV set $$ C.consume

instance CSV S.ByteString Listen where
  rowToStr set = rowToStr set . toRow
  intoCSV set  = intoCSV set =$= C.mapMaybe fromRow
  fromCSV set  = C.map toRow =$= fromCSV set

-- | Convert from a listen to a CSV row.
toRow :: Listen -> [S.ByteString]
toRow (Listen t title a) =
  [encodeUtf8 (T.pack (formatTime defaultTimeLocale fmt t))
  ,encodeUtf8 title
  ,encodeUtf8 a]

-- | Convert from a CSV row to a listen.
fromRow :: [Text] -> Maybe Listen
fromRow [_id,ts,title,artist,_release] =
  Listen <$> parseTime defaultTimeLocale fmt (T.unpack ts)
         <*> pure title
         <*> pure artist
fromRow _ = Nothing

-- | Format string for listen times.
fmt :: String
fmt = "%F %T+02"
