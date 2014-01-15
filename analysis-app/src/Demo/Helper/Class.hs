{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Demo.Helper.Class where

import Data.CSV.Conduit
import Data.Text (Text)
import Control.Exception (Exception)
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit (Manager, http, parseUrl, responseBody)
import Yesod
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Data.Proxy

class FromMapRow a where
    fromMapRow :: MapRow Text -> Either ParseError a
class ToMapRow a where
    toMapRow :: a -> MapRow Text

fromMapRow' :: (FromMapRow a, MonadThrow m) => MapRow Text -> m a
fromMapRow' = either monadThrow return . fromMapRow

data ParseError = ColumnNotFound !Text | CouldNotReadColumn !Text
    deriving (Show, Typeable)
instance Exception ParseError

class ManagerReader m where
    askManager :: m Manager
class HasManager a where
    manager :: a -> Manager
instance (HasManager a, MonadUnsafeIO m, MonadThrow m, MonadBaseControl IO m, MonadIO m) => ManagerReader (HandlerT a m) where
    askManager = do
        x <- getYesod
        return $ manager x

sourceURL :: (MonadResource m, MonadBaseControl IO m, ManagerReader m) => String -> Source m ByteString
sourceURL url = do
    req <- liftIO $ parseUrl url
    m <- lift askManager
    res <- lift $ http req m
    (src, _) <- lift $ unwrapResumable $ responseBody res
    src

class (FromMapRow (AnalysisInput params), ToMapRow (AnalysisOutput params), HasForm params) => HasAnalysis params where
    type AnalysisInput params
    type AnalysisOutput params
    analysisOf :: (MonadResource m, MonadBaseControl IO m, ManagerReader m)
               => params
               -> Conduit (AnalysisInput params) m (AnalysisOutput params)

    analysisInput :: HasManager site => params -> Source (HandlerT site IO) ByteString

analysisBSConduit
    :: (MonadResource m, MonadBaseControl IO m, FromMapRow input, ToMapRow output)
    => Conduit input m output
    -> Conduit ByteString m (Flush Builder)
analysisBSConduit inner =
        CT.decode CT.utf8
    =$= intoCSV defCSVSettings
    =$= CL.mapM fromMapRow'
    =$= inner
    =$= CL.map toMapRow
    =$= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
    =$= CL.map (Chunk . fromText)

class HasForm a where
    form :: RenderMessage site FormMessage => AForm (HandlerT site IO) a

data SomeAnalysis = forall params.
    SomeAnalysis
        (Proxy params)
        (forall site. RenderMessage site FormMessage => AForm (HandlerT site IO) params)
        (forall site. HasManager site => params -> Source (HandlerT site IO) (Flush Builder))

getSomeAnalysis :: (HasForm params, HasAnalysis params)
                => Proxy params
                -> SomeAnalysis
getSomeAnalysis params = SomeAnalysis
    params
    form
    (\params -> analysisInput params $= analysisBSConduit (analysisOf params))