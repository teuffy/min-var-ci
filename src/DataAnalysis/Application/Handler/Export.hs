{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import           Blaze.ByteString.Builder.ByteString
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
getExportR :: Text -> Handler TypedContent
getExportR ident = do
    addHeader "content-disposition" $ T.concat
        [ "attachment; filename=\""
        , T.pack (show ident) <> "-export"
        , ".csv\""
        ]
    source <- analysisSource ident
    respondSource "text/csv"
                  (source
                   $= CL.map toMapRow
                   $= (writeHeaders settings >> fromCSV settings)
                   $= CL.map (Chunk . fromByteString))
  where settings = def
