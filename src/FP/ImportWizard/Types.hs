{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Types where

import           BasicPrelude
import           Data.CSV.Conduit.Persist (CsvInvalidRow(..), CsvFormat(..))
import qualified Data.Set                 as Set

import           FP.ImportWizard.Temp

defaultIwData :: IWData
defaultIwData = IWData
    {   iwdFormat   =   IWFormatData "" IWCSVFormat
    ,   iwdSource   =   IWSourceData True Nothing Nothing Nothing
    ,   iwdTypes    =   []
    ,   iwdInvalid  =   CsvInvalidRowStop }
        
data IWData =   IWData
    {   iwdFormat  :: IWFormatData
    ,   iwdSource  :: IWSourceData
    ,   iwdTypes   :: [IWColumn]
    ,   iwdInvalid :: CsvInvalidRow
    } deriving (Read, Show, Eq)

data IWFormatData = IWFormatData
    {   iwfdName   :: Text
    ,   iwfdFormat :: IWFormat
    } deriving (Read, Show, Eq)

data IWSourceData =  IWSourceData
    {   iwsdHasHeaderRow    :: Bool
    ,   iwsdUrl             :: Maybe Text
    ,   iwsdCsvTempToken    :: Maybe TempToken
    ,   iwsdUploadTempToken :: Maybe TempToken
    } deriving (Read, Show, Eq)

data IWColumn = IWColumn
    {   iwcName     :: Text
    ,   iwcType     :: IWType
    ,   iwcOptional :: Bool
    ,   iwcDefault  :: Maybe Text
    } deriving (Read, Show, Eq)

iwFormatToCsvFormat :: IWFormat -> Maybe CsvFormat
iwFormatToCsvFormat IWCSVFormat             = Just CsvFormatCsv
iwFormatToCsvFormat IWXMLFormat             = Just CsvFormatXml
iwFormatToCsvFormat IWPostgresFormat        = Nothing
iwFormatToCsvFormat IWStockDataFeedFormat   = Nothing

data IWFormat
    =   IWCSVFormat
    |   IWXMLFormat
    |   IWPostgresFormat
    |   IWStockDataFeedFormat
    deriving (Read, Show, Eq, Enum, Bounded)

data IWType
    =   IWTextType
    |   IWIntType
    |   IWDoubleType
    |   IWDayType Text
    |   IWTimeOfDayType Text
    |   IWEnumType (Set.Set Text)
    deriving (Read, Show, Eq)
