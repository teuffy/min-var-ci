{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import           Blaze.ByteString.Builder.ByteString
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Yesod

import           DataAnalysis.Application.Foundation
import           DataAnalysis.Application.Types

-- | Export the data source to various data formats.
getExportR :: Text -> Handler TypedContent
getExportR ident = do
    fp <- getById ident
    addHeader "content-disposition" $ T.concat
        [ "attachment; filename=\""
        , T.pack (show ident) <> "-export"
        , ".csv\""
        ]
    respondSource "text/csv"
                  (sourceFile (srcPath fp) $= CL.map (Chunk . fromByteString))
