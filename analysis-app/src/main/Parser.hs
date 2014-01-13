-- | Parser for the analyzed data type. This file is a temporary workaround.

module Parser where

import           Types

import           Control.Applicative
import           Data.ByteString.Lazy (ByteString)
import           Data.CSV.Conduit
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List    as C
import           Data.Default
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           System.Locale

-- | Parse the user data source from CSV.
parseCSVSource :: ByteString -> IO (Maybe [Listen])
parseCSVSource = fmap (Just . catMaybes . map parse) . readCSV def
  where parse [_id,ts,title,artist,_release] =
          Listen <$> parseTime defaultTimeLocale "%F %T+02" (T.unpack ts)
                 <*> pure title
                 <*> pure artist
        parse _ = Nothing

-- | Read a CSV file from a byte string.
readCSV
    :: CSVSettings
    -- ^ Settings to use in deciphering stream
    -> ByteString
    -- ^ Input string
    -> IO [[Text]]
readCSV set lbs = runResourceT $ sourceLbs lbs $= intoCSV set $$ C.consume
