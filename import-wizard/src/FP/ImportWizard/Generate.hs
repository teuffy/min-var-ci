{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Generate where

import           BasicPrelude
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import           Language.Haskell.Exts
import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import           FP.ImportWizard.Types

generateCode :: IWData -> Text
generateCode IWData{..} = 
        "--\n-- This code doesn't work yet (still in progress)\n--\n\n"
    ++  Text.pack (prettyPrint $ Module
            srcloc
            (ModuleName "X") -- EKB FIXME choose an appropriate name
            languagePragmas
            Nothing
            Nothing
            imports
            decls)

  where
    languagePragmas = map (LanguagePragma srcloc . (: []) . Ident)
        [   "EmptyDataDecls"
        ,   "FlexibleContexts"
        ,   "GADTs"
        ,   "OverloadedStrings"
        ,   "QuasiQuotes"
        ,   "TemplateHaskell"
        ,   "TypeFamilies" ]

    imports = map import_ 
        [   ("Data.Text", ["Text"])
        ,   ("Data.Time.Calendar", ["Day"])
        ,   ("Data.Time.LocalTime", ["TimeOfDay"])
        ,   ("Database.Persist", [])
        ,   ("Database.Persist.Sqlite", [])
        ,   ("Database.Persist.TH", []) ]

    import_ (modul, spec) = ImportDecl
        srcloc
        (ModuleName modul)
        False False Nothing Nothing
        (case spec of
            [] -> Nothing
            xs -> Just (False, map (IAbs . Ident) xs ))

    decls =
            trace (ppShow $ concatMap columnDecls iwdTypes) (concatMap columnDecls iwdTypes)
        ++  [SpliceDecl
                srcloc
                (App
                    (App
                        (Var (UnQual (Ident "share")))
                        (List
                            [ App
                                (Var (UnQual (Ident "mkPersist")))
                                (Var (UnQual (Ident "sqlSettings")))
                            ]))
                    (QuasiQuote
                       "persistLowerCase"
                       (Text.unpack $ Text.unlines entityDef)))
            ]

    columnDecls col@IWColumn{iwcType = IWEnumType enumVals} =
        [   DataDecl
                srcloc
                DataType
                []
                (Ident $ Text.unpack $ columnDataTypeName col)
                []
                (map enumConstructor $ Set.toList enumVals)
                (map ((, []) . UnQual . Ident) ["Eq", "Ord", "Bounded", "Enum"]) ]


    columnDecls _ = []

    columnDataTypeName IWColumn{..} = iwcName -- EKB FIXME adjust capitalization/spaces

    enumConstructor enumVal = QualConDecl
        srcloc [] [] (ConDecl (Ident $ Text.unpack $ enumValName enumVal) [])

    enumValName val = val -- EKB FIXME adjust capitalization/spaces

    entityDef :: [Text]
    entityDef = ""
        :   sourceIdent -- EKB FIXME add invalid row and 'has headers' attributes
        :   map columnDef iwdTypes
        ++  ["    deriving Show"] -- EKB FIXME add any other derived classes?

    sourceIdent = iwfdName iwdFormat -- EKB FIXME adjust capitalization/spaces

    columnDef :: IWColumn -> Text
    columnDef col@IWColumn{..} = "    "
        ++  Text.justifyLeft 15 ' ' (columnName col)
        ++  " "
        ++  columnTypeName col
        ++  if iwcOptional then " Maybe" else ""
        ++  maybe "" (" " ++) (columnTypeAttribs col)
        ++  case iwcDefault of
                Just default_   ->  " default=" ++ default_ -- EKB FIXME quote/escape
                Nothing         ->  ""

    columnName IWColumn{..} = iwcName -- EKB FIXME adjust capitalization/spaces

    columnTypeName :: IWColumn -> Text
    columnTypeName col@IWColumn{..} = case iwcType of
        IWEnumType _        ->  columnDataTypeName col
        IWIntType           ->  "Int"
        IWDoubleType        ->  "Double"
        IWDayType _         ->  "Day"
        IWTimeOfDayType _   ->  "TimeOfDay"
        IWTextType          ->  "Text"

    columnTypeAttribs IWColumn{..} = case iwcType of
        IWEnumType _        ->  Nothing
        IWIntType           ->  Nothing
        IWDoubleType        ->  Nothing
        IWDayType f         ->  Just $ "format=" ++ f -- EKB FIXME quote/escape properly
        IWTimeOfDayType f   ->  Just $ "format=" ++ f -- EKB FIXME quote/escape properly
        IWTextType          ->  Nothing

    srcloc :: SrcLoc
    srcloc = error "SrcLoc"
