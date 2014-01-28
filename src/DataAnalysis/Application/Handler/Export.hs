{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import           Blaze.ByteString.Builder.ByteString
import           Data.ByteString (ByteString)
import           Data.CSV.Conduit
import           Data.Conduit

import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Yesod

import           DataAnalysis.Application.Foundation
import           DataAnalysis.Application.Analyze
import           DataAnalysis.Application.Types

-- | Export the data source to various data formats.
getExportR :: Text -> ExportType -> Handler TypedContent
getExportR ident typ = do
    source <- analysisSource ident
    case typ of
      CsvData ->
        attachmentFromSource
          (T.pack (show ident) <> "-export" <> ".csv")
          (source
           $= CL.map toMapRow
           $= (writeHeaders settings >> fromCSV settings))
  where settings = def

-- | Output an attachment from a source.
attachmentFromSource :: Text
                     -> Source (HandlerT site IO) ByteString
                     -> HandlerT site IO TypedContent
attachmentFromSource filename source = do
  addHeader "content-disposition"
            ("attachment; filename=" <> T.pack (show (T.unpack filename)))
  respondSource "text/csv"
                (source $= CL.map (Chunk . fromByteString))
