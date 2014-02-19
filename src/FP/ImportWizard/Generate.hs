{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Generate where

import           BasicPrelude
import           Data.Char                 (isAlpha, isAlphaNum, isLower,
                                            isSpace, isUpper, toLower, toUpper)
import           Data.CSV.Conduit.Persist  (csvFormatAttribName,
                                            csvFormatToAttribValue,
                                            csvInvalidRowToAttribValue,
                                            csvInvalidRowsAttribName,
                                            csvNoHeaderRowAttribName)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as Path
import           Language.Haskell.Exts

import           FP.ImportWizard.Types

generateCode :: IWData -> IO [(FilePath, Text)]
generateCode iwData@IWData{..} =
    forM    (dataTypeFiles ++
                [   ("UserAnalysis.hs"      ,   generateSkelFile iwData "skel/src" "UserAnalysis.hs")
                ,   ("UserParameters.hs"    ,   generateSkelFile iwData "skel/src" "UserParameters.hs")
                ,   ("Main.hs"              ,   generateSkelFile iwData "skel/src" "Main.hs") ]
            ) $ \(fp,ac) -> do
        (fp,) <$> ac
  where
    dataTypeFiles
        | Text.null (iwfdName iwdFormat) = []
        | otherwise =
            [   ("UserTypes.hs"         ,   return $ generateTypes iwData)
            ,   ("UserModel.hs"         ,   return $ generateModel iwData) ]

generateTypes :: IWData -> Text
generateTypes IWData{..} =
    -- EKB TODO: don't generate if no types needed
    Text.pack $ prettyPrint $ Module
            srcloc
            (ModuleName "UserTypes")
            (map pragma ["OverloadedStrings"])
            Nothing
            Nothing
            (map import_ [("Data.CSV.Conduit.Persist", [], Nothing)])
            (concatMap columnDecls iwdTypes)

  where
    columnDecls IWColumn{iwcName, iwcType = IWEnumType enumVals} =
        [ DataDecl
            srcloc
            DataType
            []
            (Ident $ toValidConIdent $ Text.unpack iwcName)
            []
            (map (enumConstructor iwcName) (Set.toList enumVals))
            (map ((, []) . UnQual . Ident) ["Read", "Show", "Eq", "Ord", "Bounded", "Enum"])
        , InstDecl
            srcloc
            []
            (UnQual (Ident "PersistField"))
            [ TyCon (UnQual (Ident $ toValidConIdent $ Text.unpack iwcName)) ]
            [ InsDecl
                (FunBind (map (match iwcName) (Set.toList enumVals)))
            , InsDecl
                 (PatBind
                     srcloc
                     (PVar (Ident "fromPersistValue"))
                     Nothing
                     (UnGuardedRhs
                        (App
                           (Var (UnQual (Ident "fromEnumPersistValue")))
                           (Var (UnQual (Ident "toPersistValue")))))
                     (BDecls []))
            ]
        , InstDecl
            srcloc
            []
            (UnQual (Ident "PersistFieldSql"))
            [ TyCon (UnQual (Ident $ toValidConIdent $ Text.unpack iwcName)) ]
            [ InsDecl
                (FunBind
                    [ Match
                        srcloc
                        (Ident "sqlType")
                        [ PWildCard ]
                        Nothing
                        (UnGuardedRhs (Con (UnQual (Ident "SqlString"))))
                        (BDecls [])
                    ])
            ]
        ]

    columnDecls _ = []

    enumConstructor t v = QualConDecl
        srcloc [] [] (ConDecl (Ident $ enumConstructorName t v) [])

    match t v = Match
        srcloc
        (Ident "toPersistValue")
        [ PApp (UnQual (Ident $ enumConstructorName t v)) [] ]
        Nothing
        (UnGuardedRhs
           (App (Con (UnQual (Ident "PersistText"))) (Lit (String $ Text.unpack v))))
        (BDecls [])

    enumConstructorName t v = toValidConIdent (Text.unpack t) ++ toValidConIdent (Text.unpack v)

generateModel :: IWData -> Text
generateModel IWData{..} =
    Text.pack $ prettyPrint $ Module
            srcloc
            (ModuleName "UserModel")
            (map pragma ["GADTs", "OverloadedStrings", "QuasiQuotes", "TemplateHaskell", "TypeFamilies"])
            Nothing
            (Just
                [   EModuleContents (ModuleName "UserModel")
                ,   EModuleContents (ModuleName "UserTypes")
                ,   EModuleContents (ModuleName "DataAnalysis.Application.Import")
                ])
            (map import_
                [   ("Data.CSV.Conduit.Persist"       , [], Nothing)
                ,   ("UserTypes"                      , [], Nothing)
                ,   ("DataAnalysis.Application.Import", [], Nothing)
                ])
            decls

  where
    decls = [ SpliceDecl
        srcloc
        (App
            (Var (UnQual (Ident "mkCsvPersist")))
            (QuasiQuote
                "persistCsv"
                (Text.unpack $ Text.unlines entityDef)))
     ]

    entityDef :: [Text]
    entityDef = ""
        :   (   Text.pack (toValidConIdent $ Text.unpack $ iwfdName iwdFormat)
                ++  " " ++ attrib csvInvalidRowsAttribName (csvInvalidRowToAttribValue iwdInvalid)
                ++  " " ++ attrib csvFormatAttribName
                            (   csvFormatToAttribValue $ fromMaybe (error "Unsupported format")
                            $   iwFormatToCsvFormat $ iwfdFormat iwdFormat )
                ++  (if iwsdHasHeaderRow iwdSource then "" else " " ++ csvNoHeaderRowAttribName))
        :   map columnDef iwdTypes
        ++  ["    deriving Show"] -- EKB TODO add any other derived classes?

    columnDef :: IWColumn -> Text
    columnDef col@IWColumn{..} = "    "
        ++  Text.justifyLeft 15 ' ' (Text.pack $ toValidVarIdent $ Text.unpack iwcName)
        ++  " "
        ++  columnTypeName col
        ++  if iwcOptional then " Maybe" else ""
        ++  maybe "" (" " ++) (columnTypeAttribs col)
        ++  case iwcDefault of
                Just default_   ->  " " ++ attrib "default" default_
                Nothing         ->  ""

    columnTypeName :: IWColumn -> Text
    columnTypeName IWColumn{..} = case iwcType of
        IWEnumType _        ->  Text.pack $ toValidConIdent $ Text.unpack iwcName
        IWIntType           ->  "Int"
        IWDoubleType        ->  "Double"
        IWDayType _         ->  "Day"
        IWTimeOfDayType _   ->  "TimeOfDay"
        IWTextType          ->  "Text"

    columnTypeAttribs IWColumn{..} = case iwcType of
        IWEnumType _        ->  Nothing
        IWIntType           ->  Nothing
        IWDoubleType        ->  Nothing
        IWDayType f         ->  Just $ attrib csvFormatAttribName f
        IWTimeOfDayType f   ->  Just $ attrib csvFormatAttribName f
        IWTextType          ->  Nothing

pragma :: String -> ModulePragma
pragma = LanguagePragma srcloc . (: []) . Ident

import_ :: (String, [String], Maybe String) -> ImportDecl
import_ (modul, spec, alias) = ImportDecl
    srcloc
    (ModuleName modul)
    False False Nothing (ModuleName <$> alias)
    (case spec of
        [] -> Nothing
        xs -> Just (False, map (IAbs . Ident) xs ))

srcloc :: SrcLoc
srcloc = error "SrcLoc"

attrib :: Text -> Text -> Text
attrib attrName value
    | isValidIdent (Text.unpack value) = attrName ++ "=" ++ value
        -- EKB TODO is this correct escaping?
    | otherwise = "\"" ++ attrName ++ "=" ++ Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" value) ++ "\""

-- EKB TODO test all of the identifier generation/checking thoroughly

isValidConIdent :: String -> Bool
isValidConIdent [] = False
isValidConIdent (a : as)
    | isUpper a     = isValidIdent as
    | otherwise     = False

-- EKB TODO check for reserved words?
isValidVarIdent :: String -> Bool
isValidVarIdent [] = False
isValidVarIdent (a : as)
    | isLower a || a == '_'     = isValidIdent as
    | otherwise                 = False

toValidConIdent :: String -> String
toValidConIdent [] = ""
toValidConIdent (a:as)
    | isLower a                 = toUpper a : toValidIdent as
    | isAlpha a                 = a         : toValidIdent as
    | isValidIdentChar a        = 'C'       : a : toValidIdent as
    | otherwise                 = "C_"     ++ toValidIdent as

-- EKB TODO check and adjust for reserved words?
toValidVarIdent :: String -> String
toValidVarIdent [] = ""
toValidVarIdent (a:as)
    | isUpper a                 = toLower a : toValidIdent as
    | isAlpha a || a == '_'     = a         : toValidIdent as
    | isValidIdentChar a        = '_'       : a : toValidIdent as
    | otherwise                 = '_'       : toValidIdent as

toValidIdent :: String -> String
toValidIdent "" = ""
toValidIdent [a]
    | isValidIdentChar a        = [a]
    | isSpace a                 = ""
    | otherwise                 = "_"
toValidIdent (a:a':as)
    | isValidIdentChar a        = a     : toValidIdent (a':as)
    | isSpace a || a == '-'     =         toValidIdent (toUpper a':as)
    | otherwise                 = '_'   : toValidIdent (a':as)

isValidIdent :: String -> Bool
isValidIdent = all isValidIdentChar

isValidIdentChar :: Char -> Bool
isValidIdentChar c = isAlphaNum c || c == '_' || c == '\''

generateSkelFile :: IWData -> FilePath -> FilePath -> IO Text
generateSkelFile IWData{..} dir modul = do
    let pmod = analysisParentModule iwdAnalysis
        pdir = Path.concat $ map Path.fromText $ Text.splitOn "." pmod
    c <- Text.decodeUtf8 <$> FS.readFile (dir </> pdir </> modul)
    return $ Text.replace (pmod ++ ".") "" c

analysisParentModule :: IWAnalysis -> Text
analysisParentModule IWCustomAnalysis   = "Skel.Custom"
analysisParentModule IWRSIAnalysis      = "Skel.RSI"
analysisParentModule IWKmerAnalysis     = "Skel.Kmer"
