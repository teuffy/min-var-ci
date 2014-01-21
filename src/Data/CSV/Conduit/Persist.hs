{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}

-- | Persistent support for CSV data.

module Data.CSV.Conduit.Persist
    (   Text
    ,   Day
    ,   TimeOfDay
    ,   mkCsvPersist
    ,   persistCsv
    ,   fromEnumPersistValue
    ,   csvIntoEntities
    ) where

import           BasicPrelude
import           Data.CSV.Conduit             (defCSVSettings, intoCSV)
import           Data.Conduit                 (Conduit, (=$=),MonadThrow)
import qualified Data.Conduit.List            as CL
import qualified Data.Map                     as Map
import           Data.Proxy
import qualified Data.Text                    as Text
import           Data.Time.Calendar           (Day)
import           Data.Time.Format             (parseTime)
import           Data.Time.LocalTime          (TimeOfDay)
import           Database.Persist             as X
import           Database.Persist.TH          as X
import           Language.Haskell.TH.Quote    (QuasiQuoter)
import           Language.Haskell.TH.Syntax   (Dec, Q)
import           System.Locale                (defaultTimeLocale)

mkCsvPersist :: [EntityDef SqlType] -> Q [Dec]
mkCsvPersist = mkPersist sqlOnlySettings{mpsGeneric = False}

persistCsv :: QuasiQuoter
persistCsv = persistLowerCase

fromEnumPersistValue
    :: (Bounded b, Enum b, Ord a, Show a)
    => (b -> a) -> a -> Either Text b
fromEnumPersistValue tpv x = case Map.lookup x m of
    Just v  -> Right v
    Nothing -> Left $ "Unexpected value; received: " ++ show x
  where
    m = Map.fromList $ map (\v -> (tpv v, v)) [minBound..maxBound]

csvIntoEntities
    :: (MonadThrow m,PersistEntity entity)
    => Proxy entity -> Conduit ByteString m (Either Text entity)
csvIntoEntities e = result

  where
    result =
      -- EKB FIXME handle header row
      intoCSV defCSVSettings
      =$= CL.map (csvRowIntoEntity $ entityDef e)
    csvRowIntoEntity :: PersistEntity entity => EntityDef SqlType -> [Text] -> Either Text entity
    csvRowIntoEntity entDef row =
        -- EKB FIXME handle mismatch in # of fields
        fromPersistValues $ zipWith toPV (entityFields entDef) row

    toPV FieldDef{..} val = case fieldSqlType of
        -- EKB FIXME don't use partial read in any of these
        -- EKB FIXME handle errors correctly
        SqlString -> PersistText val
        SqlInt64  -> PersistInt64 $ read val
        SqlReal   -> PersistDouble $ read val
        SqlDay ->
            case parseTime
                    defaultTimeLocale
                    (Text.unpack $ fromMaybe "%F" $ getAttribValue "format" fieldAttrs)
                    (Text.unpack val) of
                Just day    -> PersistDay day
                Nothing     -> error $ "invalid Day: " ++ Text.unpack val
        SqlTime ->
            -- EKB TODO allow to specify timezone?
            case parseTime
                    defaultTimeLocale
                    (Text.unpack $ fromMaybe "%T" $ getAttribValue "format" fieldAttrs)
                    (Text.unpack val) of
                Just day    -> PersistTimeOfDay day
                Nothing     -> error $ "invalid TimeOfDay: " ++ Text.unpack val
        _ -> error "not implemented yet" -- EKB TODO implement others

    getAttribValue :: Text -> [Text] -> Maybe Text
    getAttribValue _ [] = Nothing
    getAttribValue name (a:as) = case Text.stripPrefix (name ++ "=") a of
        Nothing -> getAttribValue name as
        Just s  -> Just s

--EKB TODO make a variant of derivePersistField that handles conversion
--  from CSV values instead of using Read/Show
