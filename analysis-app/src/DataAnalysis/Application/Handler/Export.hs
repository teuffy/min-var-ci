{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Export the data source to various data formats.

module DataAnalysis.Application.Handler.Export where

import           Blaze.ByteString.Builder.ByteString
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Monoid
import qualified Data.Text as T
import           Yesod

import           DataAnalysis.Application.Foundation
import           DataAnalysis.Application.Types

-- | Export the data source to various data formats.
getExportR :: Int -> String -> Handler TypedContent
getExportR ident typ = do
    GApp app _ <- getYesod
    source <- getById app ident
    addHeader "content-disposition" $ T.concat
        [ "attachment; filename=\""
        , T.pack (show ident) <> "-export"
        , ".csv\""
        ]
    respondSource
      "text/csv"
      (CL.sourceList (srcParsed source) $=
       appFromCSV app def $=
       CL.map (Chunk . fromByteString))
