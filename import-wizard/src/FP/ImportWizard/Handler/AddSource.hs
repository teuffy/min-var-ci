{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -Werror #-}

module FP.ImportWizard.Handler.AddSource where

import           Data.Conduit              (($$), ($$+-), (=$))
import           Data.Conduit.Binary       (sinkFile, sourceFile)
import qualified Data.Conduit.List         as CL
import           Data.CSV.Conduit          (Row, defCSVSettings, intoCSV)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Char                 (isAlpha, isAlphaNum, isSpace)
import           Data.Time.Calendar        (Day)
import           Data.Time.Format          (parseTime)
import           Data.Time.LocalTime       (TimeOfDay)
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as Path
import           Network.HTTP.Conduit      (http, parseUrl, responseBody,
                                            withManager)
import           Safe                      (headDef)
import           System.Directory          (getTemporaryDirectory)
import           System.IO                 (hClose, openTempFile)
import           System.Locale             (defaultTimeLocale)

import           FP.ImportWizard.Import    hiding (parseTime)
import           FP.ImportWizard.Wizard

getAddSourceR :: Handler Html
getAddSourceR = do
    wizardResult <- runWizard config
    case wizardResult of
        WizardSuccess html -> return html
        WizardCancelled -> redirect HomeR
  where
    config = (wizardConfig defaultData iwPageHandler)
        {   wcGetPageTitle = return . Just . iwPageTitle }
        -- EKB FIXME move next to IWData def
    defaultData = IWData
        {   iwdFormat   =   IWFormatData "" IWCSVFormat
        ,   iwdSource   =   IWSourceData True Nothing Nothing Nothing
        ,   iwdTypes    =   IWTypesData [] }

postAddSourceR :: Handler Html
postAddSourceR = getAddSourceR

iwPageHandler :: IWData -> IWPage -> IWWizard (WizardResult Html)

iwPageHandler oldData IWFormatPage = do

    ((res, formWidget), enctype) <- do
        let IWFormatData{..} = iwdFormat oldData
        runWizardForm $ renderTable $ IWFormatData
            <$> areq textField "Name:" (Just iwfdName)
            <*> areq (selectFieldSum formatTitle) "Format:" (Just iwfdFormat)

    case res of
        WizardFormDiscard -> continueWizard oldData
        WizardFormSave (FormSuccess newIWFD@IWFormatData{..}) ->
            continueWizard oldData{iwdFormat = newIWFD}
        WizardFormNext (FormSuccess newIWFD@IWFormatData{..}) ->
            if iwfdFormat == IWCSVFormat
                then
                    continueWizard oldData{iwdFormat = newIWFD}
                else do
                    setMessage "Only the CSV format is supported for the demo."
                    renderIWPage formWidget enctype
        _ -> renderIWPage formWidget enctype

  where
    selectFieldSum n = selectFieldList $ map (\x -> (n x, x)) [minBound .. maxBound]

iwPageHandler oldData@IWData{iwdSource = oldIwsd} IWSourcePage = do

    ((res, formWidget), enctype) <- runWizardForm $ renderTable $ SourceForm
        <$> aopt urlField "CSV data source URL:"{fsAttrs=wideFieldAttrs} (Just $ iwsdUrl oldIwsd)
        <*> aopt textareaField "or enter CSV data:"{fsAttrs=largeTextareaAttrs} (Just $ Textarea <$> iwsdCsv oldIwsd)
        <*> fileAFormOpt "or upload CSV file:"
        <*> areq checkBoxField "Has header row:" (Just $ iwsdHasHeaderRow oldIwsd)
        -- EKB TODO also specify separator and quote characters

    case res of
        WizardFormDiscard -> continueWizard oldData
        WizardFormSave (FormSuccess sf) -> continueWizard $ saveForm sf
        WizardFormNext (FormSuccess sf@SourceForm{..}) -> do
            -- EKB TODO if user uploaded a file then navigates back, next should work without having to re-upload
            let savedData = saveForm sf
                savedIwsd = iwdSource savedData
            if (maybe1 (iwsdCsv savedIwsd) + maybe1 (iwsdUrl savedIwsd) + maybe1 sfFileInfo) /= 1
                then do
                    setMessage "Exactly one data source must be specified."
                    renderIWPage formWidget enctype
                else do
                    newTempPath <- if
                                isJust sfFileInfo
                            ||  iwsdCsv savedIwsd /= iwsdCsv oldIwsd
                            ||  iwsdUrl savedIwsd /= iwsdUrl oldIwsd
                        then getFile sfFileInfo (iwsdCsv savedIwsd) (iwsdUrl savedIwsd)
                        else return $ iwsdTempPath oldIwsd
                    let newIwsd = savedIwsd{iwsdTempPath = newTempPath}
                    newData <- if newIwsd /= oldIwsd
                            then do
                                types <- deriveCsvColumns newIwsd
                                return savedData
                                    {   iwdSource = newIwsd
                                    ,   iwdTypes = IWTypesData types }
                            else return oldData
                    continueWizard newData
        _ -> renderIWPage formWidget enctype

  where

    saveForm SourceForm{..} = oldData{iwdSource = oldIwsd
        {   iwsdUrl          = stripMaybe sfUrl
        ,   iwsdCsv          = stripMaybe $ unTextarea <$> sfCsv
        ,   iwsdHasHeaderRow = sfHasHeaderRow } }

    getFile :: Maybe FileInfo -> Maybe Text -> Maybe Text -> IWWizard (Maybe String)
    getFile sfFileInfo maybeCsv maybeUrl = do
        -- EKB TODO must clean up temp files that are left due to abandoning wizard
        -- EKB TODO also catch exceptions here and clean up temp file
        tempPath <- getTempPath
        case sfFileInfo of
            Just fileInfo ->
                -- EKB TODO adjust Yesod instance's maximumContentLength as appropriate and have good error message
                liftIO $ fileMove fileInfo tempPath
            Nothing -> case maybeCsv of
                Just csv ->
                    liftIO $ FS.writeFile (Path.decodeString tempPath) $ Text.encodeUtf8 csv
                Nothing -> case maybeUrl of
                    Just url -> do
                        -- EKB TODO only download partial data
                        -- EKB TODO friendly error handling
                        req <- parseUrl $ Text.unpack url
                        liftIO $ withManager $ \mgr -> do
                            resp <- http req mgr
                            responseBody resp $$+- sinkFile tempPath
                    Nothing ->
                        error "no source should not be able to happen here"
        return $ Just tempPath

    deriveCsvColumns IWSourceData{..} =
        sourceFile (fromMaybe "" iwsdTempPath) $$
            intoCSV defCSVSettings =$ do
                columnNames <- CL.isolate (if iwsdHasHeaderRow then 1 else 0) =$
                    CL.fold deriveHeaderRow (defaultColumnNames 1)
                columnPossibleTypes <- CL.isolate analyzeRowCount =$
                    CL.fold deriveRow []
                return $ flip map (zip columnNames columnPossibleTypes) $ \(n,(ts,o)) -> IWColumn
                        -- EKB TODO make column name valid for identifier
                    {   iwcName     =   n
                    ,   iwcType     =   headDef IWTextType ts
                    ,   iwcOptional =   o
                    ,   iwcDefault  =   Nothing }

    deriveHeaderRow :: [Text] -> Row Text -> [Text]
    deriveHeaderRow _ row = row

    defaultColumnNames :: Int -> [Text]
    defaultColumnNames n = ("column" ++ show n) : defaultColumnNames (n + 1)

    deriveRow :: [([IWType],Bool)] -> Row Text -> [([IWType], Bool)]
    deriveRow rowOldPossibleTypes rowValues =
        flip map (zip (rowOldPossibleTypes ++ repeat (defaultPossibleTypes, False)) rowValues) $
                \((oldPossibleTypes, oldHasNull), val) ->
            if Text.null val
                then (oldPossibleTypes, True)
                else
                    (   catMaybes $ flip map oldPossibleTypes $ \t -> case t of
                            IWTextType          ->  Just t
                            IWIntType           ->  const t <$> (readMay val :: Maybe Int)
                            IWDoubleType        ->  const t <$> (readMay val :: Maybe Double)
                            IWDayType fmt       ->  const t <$>
                                (parseTime defaultTimeLocale (Text.unpack fmt) (Text.unpack val) :: Maybe Day)
                            IWTimeOfDayType fmt ->  const t <$>
                                (parseTime defaultTimeLocale (Text.unpack fmt) (Text.unpack val) :: Maybe TimeOfDay)
                            IWEnumType oldEnum  ->
                                if          isAlpha (Text.head val)
                                        &&  Text.all (\c -> isAlphaNum c || isSpace c || c `elem` "_-'") val
                                    then
                                        let newEnum = Set.insert (Text.strip val) oldEnum
                                        in if Set.size newEnum > maxEnumSize
                                            then Nothing
                                            else Just $ IWEnumType newEnum
                                    else Nothing
                    ,   oldHasNull )

    analyzeRowCount =   1000
    maxEnumSize     =   50

    getTempPath = liftIO $ do
        (path, h) <- flip openTempFile "sample.csv" =<< getTemporaryDirectory
        hClose h
        return path

    stripMaybe :: Maybe Text -> Maybe Text
    stripMaybe mt =
        case Text.strip $ fromMaybe "" mt of
            "" -> Nothing
            t -> Just t

    maybe1 :: Maybe a -> Int
    maybe1 = maybe 0 (const 1)

iwPageHandler oldData@IWData{iwdTypes = oldIwtd} IWTypesPage = do
    ((res, formWidget), enctype) <- runWizardForm $ \extra -> do

        columnForms <- forM (iwtdColumns oldIwtd) $ \IWColumn{..} -> do
            (nameRes, nameView) <- mreq textField "" (Just iwcName)
            (typeRes, typeView) <- mreq (selectFieldList typeFieldOptions) ""
                                        (Just $ typeFieldValue iwcType)
            (optionalRes, optionalView) <- mreq checkBoxField "" (Just iwcOptional)
            (defaultRes, defaultView) <- mopt textField "" (Just iwcDefault)
            maybeAttribField <- case iwcType of
                IWDayType f -> Just <$> mreq textField "Format:" (Just f)
                IWTimeOfDayType f -> Just <$> mreq textField "Format:" (Just f)
                IWEnumType v ->
                    Just <$> mreq textField "Values:"
                        (Just $ Text.intercalate enumValueDelimiter $ Set.toList v)
                _ -> return Nothing
            let columnRes = IWColumn
                    <$> nameRes
                    <*> typeRes
                    <*> optionalRes
                    <*> defaultRes
                columnWidget = [whamlet|
                    <tr>
                        <td>^{fvInput nameView}
                        <td>^{fvInput typeView}
                        <td align=center>^{fvInput optionalView}
                        <td>^{fvInput defaultView}
                        <td>
                            $case maybeAttribField
                                $of Just (_, v)
                                    ^{fvLabel v}^{fvInput v}
                                $of Nothing |]
            return (columnRes, columnWidget)

        let widget = [whamlet|
                #{extra}
                <table>
                    <tr>
                        <th>Name
                        <th>Type
                        <th>Optional?
                        <th>Default
                        <th>Attributes
                    $forall (_, columnWidget) <- columnForms
                        ^{columnWidget} |]
        return (formResultRows columnForms, widget)

    case res of
        WizardFormDiscard -> continueWizard oldData
        WizardFormSave (FormSuccess newColumns) -> saveAndContinue newColumns
        WizardFormNext (FormSuccess newColumns) -> saveAndContinue newColumns
        _ -> renderIWPage formWidget enctype

  where

    typeFieldOptions = nub $ map typeFieldOption defaultPossibleTypes

    typeFieldValue = snd . typeFieldOption

    typeFieldOption :: IWType -> (Text, IWType)
    typeFieldOption IWTextType           =   ("Text"         , IWTextType)
    typeFieldOption IWIntType            =   ("Int"          , IWIntType)
    typeFieldOption IWDoubleType         =   ("Double"       , IWDoubleType)
    typeFieldOption (IWEnumType _)       =   ("enumeration"  , IWEnumType Set.empty)
    typeFieldOption (IWDayType _)        =   ("Day"          , IWDayType "")
    typeFieldOption (IWTimeOfDayType _)  =   ("TimeOfDay"    , IWTimeOfDayType "")

    saveAndContinue newColumns = do
        let newData = oldData{iwdTypes = oldIwtd{iwtdColumns = newColumns}}
        continueWizard newData

    enumValueDelimiter = ","

    formResultRows = mconcat . map ((pure <$>) . fst)

iwPageHandler _ IWInvalidPage   =   error "Not implemented yet!" -- EKB TODO
iwPageHandler _ IWReviewPage    =   undefined -- EKB TODO
iwPageHandler _ IWGeneratedPage =   undefined -- EKB TODO

wideFieldAttrs :: [(Text, Text)]
wideFieldAttrs = [("size", "65"), ("style", "width:65ex")]

largeTextareaAttrs :: [(Text, Text)]
largeTextareaAttrs = ("rows", "10") : wideFieldAttrs

data SourceForm = SourceForm
    {   sfUrl          ::  Maybe Text
    ,   sfCsv          ::  Maybe Textarea
    ,   sfFileInfo     ::  Maybe FileInfo
    ,   sfHasHeaderRow ::  Bool }

renderIWPage :: Widget -> Enctype -> IWWizard (WizardResult Html)
renderIWPage formWidget enctype = do
    maybeTitle <- getWizardPageTitle
    navWidget <- getWizardNavigationsWidget
    html <- liftWizardHandler $ defaultLayout $ do
        setTitle $ "Import wizard" ++ maybe "" (": " ++) maybeTitle
        [whamlet|
            <form method=post action@{HomeR} enctype=#{enctype}>
                <table>
                    ^{formWidget}
                    <tr>
                        <td colspan=2>
                            ^{navWidget}
        |]
    -- EKB TODO this probably shouldn't have to wrap its output in WizardResult
    return $ WizardSuccess html

type IWWizard a = WizardT IWData IWPage Handler Html a

iwPageTitle :: IWPage -> Html
iwPageTitle IWFormatPage      =   "Name and data format"
iwPageTitle IWSourcePage      =   "Data source"
iwPageTitle IWTypesPage       =   "Column data types"
iwPageTitle IWInvalidPage     =   "Invalid data handling"
iwPageTitle IWReviewPage      =   "Review"
iwPageTitle IWGeneratedPage   =   "Generated code"

data IWPage
    =   IWFormatPage
    |   IWSourcePage
    |   IWTypesPage
    |   IWInvalidPage
    |   IWReviewPage
    |   IWGeneratedPage
    deriving (Read, Show, Eq, Bounded, Enum)

data IWData =   IWData
    {   iwdFormat :: IWFormatData
    ,   iwdSource :: IWSourceData
    ,   iwdTypes  :: IWTypesData
    } deriving (Read, Show, Eq)

data IWFormatData = IWFormatData
    {   iwfdName   :: Text
    ,   iwfdFormat :: IWFormat
    } deriving (Read, Show, Eq)

data IWSourceData =  IWSourceData
    {   iwsdHasHeaderRow :: Bool
    ,   iwsdUrl          :: Maybe Text
    ,   iwsdCsv          :: Maybe Text
        -- EKB TODO security risk to store full path here!
    ,   iwsdTempPath     :: Maybe String
    } deriving (Read, Show, Eq)

data IWTypesData = IWTypesData
    {   iwtdColumns :: [IWColumn]
    } deriving (Read, Show, Eq)

data IWColumn = IWColumn
    {   iwcName     :: Text
    ,   iwcType     :: IWType
    ,   iwcOptional :: Bool
    ,   iwcDefault  :: Maybe Text
    } deriving (Read, Show, Eq)

formatTitle :: IWFormat -> Text
formatTitle IWCSVFormat             =   "CSV file"
formatTitle IWPostgresFormat        =   "Postgres database"
formatTitle IWStockDataFeedFormat   =   "Stock data feed"

data IWFormat
    =   IWCSVFormat
    |   IWPostgresFormat
    |   IWStockDataFeedFormat
    deriving (Read, Show, Eq, Enum, Bounded)

-- Sort these in order of preference if a field could have more than one type
defaultPossibleTypes :: [IWType]
defaultPossibleTypes =
    [   IWIntType
    ,   IWDoubleType
        -- EKB TODO add more date and time formats?
    ,   IWDayType "%-m/%-d/%y"              -- "4/20/77"
    ,   IWDayType "%-m/%-d/%Y"              -- "4/20/1977"
    ,   IWDayType "%F"                      -- "1977-04-20"
    ,   IWDayType "%-d-%b-%Y"               -- "20-Apr-1977"
    ,   IWDayType "%-d-%b-%y"               -- "20-Apr-77"
    ,   IWDayType "%Y-%b-%-d"               -- "1977-Apr-20"
    ,   IWDayType "%y-%b-%-d"               -- "77-Apr-20"
    ,   IWDayType "%-d %B, %Y"              -- "20 April, 1977"
    ,   IWDayType "%-d %B, %y"              -- "20 April, 77"
    ,   IWDayType "%B %-d, %Y"              -- "April 20, 1977"
    ,   IWDayType "%B %-d, %y"              -- "April 20, 77"
    ,   IWTimeOfDayType "%-H:%-M"           -- "23:45"
    ,   IWTimeOfDayType "%-I:%-M %p"        -- "11:45 pm"
    ,   IWTimeOfDayType "%-I:%-M %P"        -- "11:45 PM"
    ,   IWTimeOfDayType "%-I:%-M%p"         -- "11:45pm"
    ,   IWTimeOfDayType "%-I:%-M%P"         -- "11:45PM"
    ,   IWTimeOfDayType "%-H:%-M:%-S"       -- "23:45:02"
    ,   IWTimeOfDayType "%-I:%-M:%-S %p"    -- "12:45:02 pm"
    ,   IWTimeOfDayType "%-I:%-M:%-S %P"    -- "12:45:02 PM"
    ,   IWTimeOfDayType "%-I:%-M:%-S%p"     -- "12:45:02pm"
    ,   IWTimeOfDayType "%-I:%-M:%-S%P"     -- "12:45:02PM"
    ,   IWEnumType Set.empty
    ,   IWTextType ]

data IWType
    =   IWTextType
    |   IWIntType
    |   IWDoubleType
    |   IWDayType Text
    |   IWTimeOfDayType Text
    |   IWEnumType (Set.Set Text)
    deriving (Read, Show, Eq)
