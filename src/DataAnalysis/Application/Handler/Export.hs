{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import           Blaze.ByteString.Builder
import           Data.CSV.Conduit
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Double.Conversion.Text
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.XML.Types
import           Text.XML.Stream.Render
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
          (fname "csv")
          "text/csv"
          (source
           $= CL.map toMapRow
           $= (writeHeaders settings >> fromCSV settings)
           $= CL.map fromByteString)
        where settings = def
      XmlData ->
        attachmentFromSource
          (fname "xml")
          "application/xml"
          (source
           $= toXmlRows renderDataPoint
           $= renderBuilder settings)
        where settings = def
  where fname ext = ident <> "-export." <> ext

-- | Render a data point to XML events.
renderDataPoint :: Monad m => DataPoint -> Producer m Event
renderDataPoint (DP2 dp) =
  do with "label" (text (_d2dLabel dp))
     with "value" (text (toShortest (_d2dValue dp)))
     maybe (return ()) (with "label" . text) (_d2dGroup dp)
  where text = yield . EventContent . ContentText
renderDataPoint (DP3 (D3D x y z)) =
  do with "x" (text (tshow (fromIntegral x)))
     with "y" (text (tshow (fromIntegral y)))
     with "z" (text (tshow z))
  where text = yield . EventContent . ContentText
        tshow = T.pack . show

--------------------------------------------------------------------------------
-- Utilities

-- | Output an attachment from a source.
attachmentFromSource :: Text
                     -> ContentType
                     -> Source (HandlerT site IO) Builder
                     -> HandlerT site IO TypedContent
attachmentFromSource filename contentType source = do
  addHeader "content-disposition"
            ("attachment; filename=" <> T.pack (show (T.unpack filename)))
  respondSource contentType
                (source $= CL.map Chunk)

-- | Render to an XML document of rows.
toXmlRows :: Monad m => (row -> Conduit row m Event) -> Conduit row m Event
toXmlRows renderRow =
  do yield EventBeginDocument
     with "rows"
          (awaitForever (with "row" . renderRow))
     yield EventEndDocument

-- | With opening/closing tags for the given name, render the inner
-- conduit inside it.
with :: Monad m => Name -> Conduit void m Event -> Conduit void m Event
with name inner =
  do yield (EventBeginElement name [])
     inner
     yield (EventEndElement name)
