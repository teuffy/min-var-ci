{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Temp where

import           BasicPrelude
import           Data.Char                       (isHexDigit)
import qualified Data.Text                       as Text
import qualified Data.UUID                       as UUID
import qualified Data.UUID.V4                    as UUID
import qualified Filesystem.Path.CurrentOS       as Path
import           Web.PathPieces                  (PathPiece (..))
import           System.Directory                (getTemporaryDirectory)
import qualified Filesystem                      as FS

newTempToken :: (MonadIO m) => m (TempToken, FilePath)
newTempToken = liftIO $ do
    tempDir <- _getTempDir
    token <- UUID.toString <$> UUID.nextRandom
    return  (   TempToken token
            ,   tempDir </> Path.decodeString token)

getTempTokenPath :: (MonadIO m) => TempToken -> m FilePath
getTempTokenPath (TempToken tokenStr) = liftIO $
    -- EKB FIXME needs tests
    if all (\c -> isHexDigit c || c == '-') tokenStr
        then do
            tempDir <- _getTempDir
            return $ tempDir </> Path.decodeString tokenStr
        else error "getTempTokenPath: invalid temp token"

_getTempDir :: IO FilePath
_getTempDir = do
    -- %XXX "import-wizard" shouldn't be here
    d <- (</> "import-wizard") . Path.decodeString <$> getTemporaryDirectory
    FS.createTree d
    return d

-- %XXX make this type opaque
newtype TempToken = TempToken String
    deriving (Read, Show, Eq)

instance PathPiece TempToken where
    toPathPiece (TempToken s) = Text.pack s
                        -- %XXX should check for valid token
    fromPathPiece = Just . TempToken . Text.unpack
