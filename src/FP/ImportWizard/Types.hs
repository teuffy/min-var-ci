module FP.ImportWizard.Types where

import BasicPrelude
import qualified Data.Set                     as Set

import FP.ImportWizard.Temp
    
data IWData =   IWData
    {   iwdFormat  :: IWFormatData
    ,   iwdSource  :: IWSourceData
    ,   iwdTypes   :: [IWColumn]
    ,   iwdInvalid :: IWInvalid
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

data IWFormat
    =   IWCSVFormat
    |   IWPostgresFormat
    |   IWStockDataFeedFormat
    deriving (Read, Show, Eq, Enum, Bounded)

data IWInvalid
    =   IWInvalidStop
    |   IWInvalidSkip
    |   IWInvalidDefault
    deriving (Read, Show, Eq, Enum, Bounded)

data IWType
    =   IWTextType
    |   IWIntType
    |   IWDoubleType
    |   IWDayType Text
    |   IWTimeOfDayType Text
    |   IWEnumType (Set.Set Text)
    deriving (Read, Show, Eq)