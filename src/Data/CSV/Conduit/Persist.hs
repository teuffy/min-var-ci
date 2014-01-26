{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}

-- | Persistent support for CSV data.

{-EKB TODO

Review notes from @snoyberg:

* fromEnumPersistValue is now, high-level code. However, it's not going to be as efficient as possible, since the lookup map is going to be recomputed each time the function is called. Even though it's a bit uglier, the highest efficiency will probably come from:
    * Creating a CAF[1] with the lookup mapping.
    * Specializing to the case where the lookup key is a `Text`.
    * Switch to a HashMap instead of a Map.
    * If we were really going to go crazy on performance, using some kind of Text trie would possibly be more efficient.
    * I *don't* think that this kind of optimization is at all a priority, and definitely not necessary for the demo, but I thought now's a good time to start going over optimization techniques like this.
* csvRowIntoEntity assumes that the incoming field order is the same as the original field order. While this seems to be the only option in the case of a CSV file without headers, a CSV file with headers should instead respect them, and deal with rearranged columns appropriately.
* Another efficiency comment: it looks to me like a Day field will currently require parsing all of the attributes for each and every row traversal, as opposed to caching that lookup from the first call. Rearranging the code a bit should address that problem; in particular, you'd need to do something like `let fieldParsers = map toFieldParsers $ entityFields entDef` and then zip *that* with the row.
    * Similarly, when implementing the header-respecting lookup, the best thing would be to look at the headers once and rearrange the order of those field parsers to match the format of the CSV file. That should also help simplify the code for dealing with both the case of headers and non-headers: if there's no header, simply don't perform the rearranging.
-}

module Data.CSV.Conduit.Persist
    (   Text
    ,   Day
    ,   TimeOfDay
    ,   CsvInvalidRow(..)
    ,   mkCsvPersist
    ,   persistCsv
    ,   fromEnumPersistValue
    ,   csvIntoEntities
    ,   attribValueToCsvInvalidRow
    ,   csvInvalidRowsAttribName
    ,   csvNoHeaderRowAttribName
    ,   csvInvalidRowToAttribValue
    ) where

import           BasicPrelude
import           Data.CSV.Conduit             (defCSVSettings, intoCSV)
import           Data.Conduit                 (Conduit, (=$=), MonadThrow, await)
import qualified Data.Conduit.List            as CL
import qualified Data.Map                     as Map
import           Data.Proxy (Proxy)
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
mkCsvPersist = mkPersist sqlOnlySettings{mpsGeneric = False, mpsGenerateLenses = True}

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
csvIntoEntities p =
    intoCSV defCSVSettings
        =$= do
            if hasHeaderRow
                then void $ await
                else return ()
            CL.map csvRowIntoEntity
  where
    hasHeaderRow = not $ csvNoHeaderRowAttribName `elem` entityAttrs entDef
      
    csvRowIntoEntity :: PersistEntity entity => [Text] -> Either Text entity
    csvRowIntoEntity row =
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

    entDef :: EntityDef SqlType
    entDef = entityDef p

attribValueToCsvInvalidRow :: (Eq a, IsString a) => a -> Maybe CsvInvalidRow
attribValueToCsvInvalidRow v =
    lookup v $ map (\x -> (csvInvalidRowToAttribValue x, x)) [minBound..maxBound]

csvInvalidRowsAttribName :: Text
csvInvalidRowsAttribName = "invalidRows"

csvNoHeaderRowAttribName :: Text
csvNoHeaderRowAttribName = "noHeaderRow"
    
csvInvalidRowToAttribValue :: IsString a => CsvInvalidRow -> a
csvInvalidRowToAttribValue CsvInvalidRowStop     = "stop"
csvInvalidRowToAttribValue CsvInvalidRowSkip     = "skip"
csvInvalidRowToAttribValue CsvInvalidRowDefault  = "default"

data CsvInvalidRow
    =   CsvInvalidRowStop
    |   CsvInvalidRowSkip
    |   CsvInvalidRowDefault
    deriving (Read, Show, Eq, Enum, Bounded)

--EKB TODO make a variant of derivePersistField that handles conversion
--  from CSV values instead of using Read/Show
