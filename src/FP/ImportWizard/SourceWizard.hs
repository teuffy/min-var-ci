{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.SourceWizard where

import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Char                    (isAlpha, isAlphaNum, isSpace)
import           Data.Conduit                 (Sink, ($$), ($$+-), (=$), (=$=))
import           Data.Conduit.Binary          (sourceFile)
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit             (Row, defCSVSettings, intoCSV)
import           Data.CSV.Conduit.Persist     (CsvInvalidRow (..),
                                               xmlEventsIntoCsvRows)
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time.Calendar           (Day)
import           Data.Time.Format             (parseTime)
import           Data.Time.LocalTime          (TimeOfDay)
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as Path
import           Network.HTTP.Conduit         (http, parseUrl, responseBody,
                                               withManager)
import           Safe                         (headDef, readMay)
import           System.Locale                (defaultTimeLocale)
import           Text.XML.Stream.Parse        (def, parseBytes)

import           FP.ImportWizard.Generate
import           FP.ImportWizard.Import       hiding (parseTime)
import           FP.ImportWizard.Temp
import           FP.ImportWizard.Types
import           FP.ImportWizard.Wizard

iwPageHandler :: IWData -> IWPage -> IWWizard (WizardResult IWData Html)

iwPageHandler oldData IWFormatPage = do
    ((res, formWidget), enctype) <- do
        let IWFormatData{..} = iwdFormat oldData
        runWizardForm $ renderTable $ IWFormatData
            <$> areq textField "Name: " (Just iwfdName)
            <*> areq (selectFieldList $ optionsListSum formatTitle)
                    "Format: " (Just iwfdFormat)
    case res of
        WizardFormProcess (FormSuccess newIWFD@IWFormatData{..})
            | not (iwfdFormat `elem` [IWCSVFormat, IWXMLFormat]) -> do
                setMessage "Only the CSV and XML formats are supported for the demo."
                renderIWPage formWidget enctype
            | not (isValidConIdent $ Text.unpack iwfdName) -> do
                setMessage $ toHtml $ "Name must be a valid Haskell type identifier: " ++ validConIdentDesc ++ "."
                renderIWPage formWidget enctype
            | otherwise ->
                continueWizard oldData{iwdFormat = newIWFD}
        WizardFormContinue (FormSuccess newIWFD) ->
            continueWizard oldData{iwdFormat = newIWFD}
        WizardFormContinue _ ->
            continueWizard oldData
        _ ->
            renderIWPage formWidget enctype

iwPageHandler oldData@IWData{iwdSource = oldIwsd, iwdFormat = IWFormatData{..}} IWSourcePage = do

    defaultContent <- liftIO $ getContent oldIwsd
    let csvOrXml = formatTitle iwfdFormat
    ((res, formWidget), enctype) <- runWizardForm $ renderTable $ SourceForm
        <$> aopt urlField ""{fsLabel = fromString $ Text.unpack $ csvOrXml ++ " data source URL: ", fsAttrs = wideFieldAttrs} (Just $ iwsdUrl oldIwsd)
        <*> aopt textareaField ""{fsLabel = fromString $ Text.unpack $ "or enter " ++ csvOrXml ++ " data: ", fsAttrs = largeTextareaAttrs} (Just $ Textarea <$> defaultContent)
        <*> fileAFormOpt ""{fsLabel = fromString $ Text.unpack $ "or upload " ++ csvOrXml ++ " file: "}
        <*> if iwfdFormat == IWCSVFormat
                then areq checkBoxField "Has header row: " (Just $ iwsdHasHeaderRow oldIwsd)
                else pure True
        -- EKB TODO also specify separator and quote characters

    case res of
        WizardFormProcess (FormSuccess sf@SourceForm{..}) -> do
            savedData@IWData{iwdSource = newIwsd} <- liftIO $ saveForm sf
            if (maybe1 (iwsdCsvTempToken newIwsd) + maybe1 (iwsdUrl newIwsd) + maybe1 (iwsdUploadTempToken newIwsd)) /= 1
                then do
                    setMessage "Exactly one data source must be specified."
                    renderIWPage formWidget enctype
                else if newIwsd /= oldIwsd
                    then do
                        maybeTypes <- liftIO $ deriveColumns newIwsd
                        case maybeTypes of
                            Nothing -> continueWizard savedData
                            Just types -> continueWizard savedData{iwdTypes = types}
                    else continueWizard savedData
        WizardFormContinue (FormSuccess sf) -> continueWizard =<< liftIO (saveForm sf)
        WizardFormContinue _ -> continueWizard oldData
        _ -> renderIWPage formWidget enctype

  where

    -- EKB TODO: generalize the URL/textarea/upload stuff into Wizard

    saveForm SourceForm{..} = do
        -- EKB TODO must clean up temp files that are left due to abandoning wizard
        -- EKB TODO also catch exceptions here and clean up temp file
        let saveUrl = stripMaybe sfUrl
        saveCsvTempToken <- case stripMaybe $ unTextarea <$> sfCsv of
            Nothing -> return Nothing
            Just csv -> do
                oldCsv <- getContent oldIwsd
                if Just csv == oldCsv
                    then return $ iwsdCsvTempToken oldIwsd
                    else do
                        (tempToken, tempPath) <- newTempToken
                        FS.writeFile tempPath $ Text.encodeUtf8 csv
                        return $ Just tempToken
        saveUploadTempToken <- case sfFileInfo of
            Nothing ->
                return $ if isNothing saveUrl && isNothing saveCsvTempToken
                    then iwsdUploadTempToken oldIwsd
                    else Nothing
            Just fileInfo -> do
                (tempToken, tempPath) <- newTempToken
                -- EKB TODO adjust Yesod instance's maximumContentLength as appropriate and have good error message
                fileMove fileInfo (Path.encodeString tempPath)
                return $ Just tempToken
        return oldData{iwdSource = oldIwsd
            {   iwsdUrl             =   saveUrl
            ,   iwsdCsvTempToken    =   saveCsvTempToken
            ,   iwsdUploadTempToken =   saveUploadTempToken
            ,   iwsdHasHeaderRow    =   sfHasHeaderRow }}

    getContent :: IWSourceData -> IO (Maybe Text)
    getContent IWSourceData{..} =
        case iwsdCsvTempToken of
            Nothing -> return Nothing
            Just csvTempToken -> do
                tempPath <- getTempTokenPath csvTempToken
                (Just . Text.decodeUtf8) <$> FS.readFile tempPath

    deriveColumns iwsd =
        intoSink iwsd $ intoRows =$ do
            columnNames <- CL.isolate (if iwsdHasHeaderRow iwsd then 1 else 0) =$
                CL.fold deriveHeaderRow (defaultColumnNames 1)
            columnPossibleTypes <- CL.isolate analyzeRowCount =$
                CL.fold deriveRow []
            return $ Just $ flip map (zip columnNames columnPossibleTypes) $
                    \(n,(ts,o)) -> IWColumn
                {   iwcName     =   Text.pack $ toValidVarIdent $ Text.unpack n
                ,   iwcType     =   headDef IWTextType ts
                ,   iwcOptional =   o
                ,   iwcDefault  =   Nothing }

    intoRows = case iwfdFormat of
        IWCSVFormat -> intoCSV defCSVSettings
        IWXMLFormat -> parseBytes def =$= xmlEventsIntoCsvRows
        _ -> error $ Text.unpack $ "Unsupported format for demo: " ++ formatTitle iwfdFormat

    intoSink :: IWSourceData -> Sink ByteString (ResourceT IO) (Maybe a) -> IO (Maybe a)
    intoSink IWSourceData{..} sink =
        case iwsdUrl of
            Just url -> do
                req <- parseUrl $ Text.unpack url
                withManager $ \mgr -> do
                    resp <- http req mgr
                    responseBody resp $$+- sink
            Nothing -> case iwsdCsvTempToken of
                Just csvTempToken -> do
                    tempPath <- getTempTokenPath csvTempToken
                    runResourceT $ sourceFile (Path.encodeString tempPath) $$ sink
                Nothing -> case iwsdUploadTempToken of
                    Just uploadTempToken -> do
                        tempPath <- getTempTokenPath uploadTempToken
                        runResourceT $ sourceFile (Path.encodeString tempPath) $$ sink
                    Nothing -> return Nothing

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
                            IWIntType           ->  const t <$> (Safe.readMay $ Text.unpack val :: Maybe Int)
                            IWDoubleType        ->  const t <$> (Safe.readMay $ Text.unpack val :: Maybe Double)
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

    -- EKB TODO: make row count customizable
    analyzeRowCount =   1000

    maxEnumSize     =   50

    stripMaybe :: Maybe Text -> Maybe Text
    stripMaybe mt =
        case Text.strip $ fromMaybe "" mt of
            "" -> Nothing
            t -> Just t

    maybe1 :: Maybe a -> Int
    maybe1 = maybe 0 (const 1)

iwPageHandler oldData@IWData{iwdTypes = oldColumns} IWTypesPage = do

    ((res, formWidget), enctype) <- runWizardForm (makeForm oldColumns)

    case res of
        WizardFormProcess (FormSuccess columnForms) -> do
            let newData@IWData{iwdTypes = newColumns} = saveForm columnForms
                errors = catMaybes $ flip map newColumns $ \col@IWColumn{..} ->
                    if not (isValidVarIdent $ Text.unpack iwcName)
                        then Just (col, "'s name must be a valid Haskell variable identifier: " ++ validVarIdentDesc ++ ".")
                        else case iwcType of
                            IWEnumType s
                                |   Set.null s  -> Just (col, "needs at least one enumeration value.")
                                |   otherwise   -> Nothing
                            IWDayType "" -> Just (col, "needs a date format.")
                            IWTimeOfDayType "" -> Just (col, "needs a time format.")
                            _ -> Nothing
            case errors of
                [] ->
                    continueWizard newData
                ((IWColumn{..}, msg):_) -> do
                    setMessage $ "Column " ++ toHtml iwcName ++ " " ++ toHtml msg
                    renderIWPage formWidget enctype
        WizardFormContinue (FormSuccess columnForms) ->
            continueWizard $ saveForm columnForms
        WizardFormContinue _ ->
            continueWizard oldData
        WizardFormRender (FormSuccess columnForms) -> do
            let newData = saveForm columnForms
            putWizardData newData
            (formWidget', enctype') <- liftWizardHandler $
                generateFormPost (makeForm (iwdTypes newData))
            renderIWPage formWidget' enctype'
        _ ->
            renderIWPage formWidget enctype

  where

    makeForm columns extra = do

        columnForms <- forM columns $ \IWColumn{..} -> do
            (nameRes, nameView) <- mreq textField "" (Just iwcName)
            (typeRes, typeView) <- mreq (selectFieldList typeFieldOptions)
                                        ""{fsAttrs = [("onchange", "this.form.submit()")]}
                                        (Just $ typeFieldValue iwcType)
            (optionalRes, optionalView) <- mreq checkBoxField "" (Just iwcOptional)
            (defaultRes, defaultView) <- mopt textField "" (Just iwcDefault)
            maybeAttribField <- case iwcType of
                IWDayType f ->
                    Just <$> mopt textField "Format: " (Just $ Just f)
                IWTimeOfDayType f ->
                    Just <$> mopt textField "Format: " (Just $ Just f)
                IWEnumType v ->
                    Just <$> mopt textField "Values: "
                        (Just $ Just $ Text.intercalate enumValueDelimiter $ Set.toList v)
                _ ->
                    return Nothing
            let columnRes = ColumnForm
                    <$> (IWColumn
                        <$> nameRes
                        <*> typeRes
                        <*> optionalRes
                        <*> defaultRes)
                    <*> case maybeAttribField of
                            Just (attribRes, _) -> attribRes
                            Nothing -> pure Nothing
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

    saveForm :: [ColumnForm] -> IWData
    saveForm columnForms =
        oldData{iwdTypes = zipWith (curry updateColumn) oldColumns columnForms }

    updateColumn :: (IWColumn, ColumnForm) -> IWColumn
    updateColumn
            (   IWColumn{iwcType = oldIwcType}
            ,   ColumnForm{cfAttrib, cfColumn = newCol@IWColumn{iwcType = newIwcType}}) =
        newCol{iwcType = updatedIwcType}
      where
        updatedIwcType = if typeFieldValue oldIwcType == typeFieldValue newIwcType
            then case newIwcType of
                IWEnumType _ -> IWEnumType $
                    Set.fromList $ map Text.strip $ Text.splitOn enumValueDelimiter $ fromMaybe "" cfAttrib
                IWDayType _ -> IWDayType $ fromMaybe "" cfAttrib
                IWTimeOfDayType _ -> IWTimeOfDayType $ fromMaybe "" cfAttrib
                _ -> newIwcType
            else newIwcType

    typeFieldOptions :: [(Text, IWType)]
    typeFieldOptions = nub $ map typeFieldOption defaultPossibleTypes

    typeFieldValue :: IWType -> IWType
    typeFieldValue = snd . typeFieldOption

    typeFieldOption :: IWType -> (Text, IWType)
    typeFieldOption IWTextType           =   ("Text"         , IWTextType)
    typeFieldOption IWIntType            =   ("Int"          , IWIntType)
    typeFieldOption IWDoubleType         =   ("Double"       , IWDoubleType)
    typeFieldOption (IWEnumType _)       =   ("enumeration"  , IWEnumType Set.empty)
    typeFieldOption (IWDayType _)        =   ("Day"          , IWDayType "")
    typeFieldOption (IWTimeOfDayType _)  =   ("TimeOfDay"    , IWTimeOfDayType "")

    formResultRows :: [(FormResult f, w)] -> FormResult [f]
    formResultRows = mconcat . map ((pure <$>) . fst)

    enumValueDelimiter = ","

iwPageHandler oldData IWInvalidPage = do
    ((res, formWidget), enctype) <-
        runWizardForm $ renderTable $ areq (radioFieldList $ optionsListSum invalidTitle)
            "Invalid row handling: " (Just $ iwdInvalid oldData)
    case res of
        WizardFormProcess (FormSuccess newInvalid) ->
            continueWizard oldData{iwdInvalid = newInvalid}
        WizardFormContinue (FormSuccess newInvalid) ->
            continueWizard oldData{iwdInvalid = newInvalid}
        WizardFormContinue _ ->
            continueWizard oldData
        _ ->
            renderIWPage formWidget enctype

iwPageHandler oldData IWAnalysisPage = do
    ((res, formWidget), enctype) <-
        runWizardForm $ renderTable $ areq (radioFieldList $ optionsListSum analysisTitle)
            "Analysis: " (Just $ iwdAnalysis oldData)
    case res of
        WizardFormProcess (FormSuccess newAnalysis) ->
            continueWizard oldData{iwdAnalysis = newAnalysis}
        WizardFormContinue (FormSuccess newAnalysis) ->
            continueWizard oldData{iwdAnalysis = newAnalysis}
        WizardFormContinue _ ->
            continueWizard oldData
        _ ->
            renderIWPage formWidget enctype

iwPageHandler data_ IWReviewPage = do
    submitted <- getIsPageSubmitted
    if submitted
        then continueWizard data_
        else do
            generatedCode <- liftIO $ generateCode data_
            let widget = toWidget [shamlet|
                    $forall (path, code) <- generatedCode
                        <h3>#{Path.encodeString path}
                        <pre>#{code}
                    |]
            -- EKB TODO need to hard-code `Multipart` when not using runWizardForm?
            renderIWPage widget Multipart

renderIWPage :: Widget -> Enctype -> IWWizard (WizardResult IWData Html)
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
    return $ WizardSuccess html

validConIdentDesc :: Text
validConIdentDesc = "Starts with an upper-case letter, followed by letters, numbers, underscores, and apostrophes (e.g. `MyConstructor`)"

validVarIdentDesc :: Text
validVarIdentDesc = "Starts with a lower-case letter or underscore, followed by letters, numbers, underscores, and apostrophes (e.g. `myVariable`)"

optionsListSum :: (Bounded a, Enum a) => (a -> Text) -> [(Text, a)]
optionsListSum n = map (\x -> (n x, x)) [minBound .. maxBound]

wideFieldAttrs :: [(Text, Text)]
wideFieldAttrs = [("size", "65"), ("style", "width:65ex")]

largeTextareaAttrs :: [(Text, Text)]
largeTextareaAttrs = ("rows", "10") : wideFieldAttrs

data SourceForm = SourceForm
    {   sfUrl          ::  Maybe Text
    ,   sfCsv          ::  Maybe Textarea
    ,   sfFileInfo     ::  Maybe FileInfo
    ,   sfHasHeaderRow ::  Bool }

data ColumnForm = ColumnForm
    {   cfColumn :: IWColumn
    ,   cfAttrib :: Maybe Text }
    deriving (Show)

type IWWizard a = WizardT IWData IWPage Handler Html a

iwPageTitle :: IWPage -> Html
iwPageTitle IWFormatPage      =   "Name and data format"
iwPageTitle IWSourcePage      =   "Data source"
iwPageTitle IWTypesPage       =   "Column data types"
iwPageTitle IWInvalidPage     =   "Invalid data handling"
iwPageTitle IWAnalysisPage    =   "Select analysis"
iwPageTitle IWReviewPage      =   "Review generated code"

data IWPage
    =   IWFormatPage
    |   IWSourcePage
    |   IWTypesPage
    |   IWInvalidPage
    |   IWAnalysisPage
    |   IWReviewPage
    deriving (Read, Show, Eq, Bounded, Enum)

invalidTitle :: CsvInvalidRow -> Text
invalidTitle CsvInvalidRowStop      =   "Stop processing"
invalidTitle CsvInvalidRowSkip      =   "Skip and ignore the row"
invalidTitle CsvInvalidRowDefault   =   "Set invalid columns to default values"

formatTitle :: IWFormat -> Text
formatTitle IWCSVFormat             =   "CSV"
formatTitle IWXMLFormat             =   "XML"
formatTitle IWPostgresFormat        =   "Postgres database"
formatTitle IWStockDataFeedFormat   =   "Stock data feed"

analysisTitle :: IWAnalysis -> Text
analysisTitle IWCustomAnalysis = "Custom"
analysisTitle IWRSIAnalysis = "RSI"

defaultPossibleTypes :: [IWType]
defaultPossibleTypes =
    -- Sorted in order of preference if a field could have more than one type
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
