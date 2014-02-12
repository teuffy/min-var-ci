{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Datatypes for analysis application API.

module DataAnalysis.Application.Types where

import           Control.Exception (Exception)
import           Control.Lens.TH
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.CSV.Conduit.Persist
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Data
import           Data.Default
import           Data.Function
import           Data.IORef
import           Data.Proxy
import           Data.Time
import           Network.HTTP.Conduit (Manager, http, parseUrl, responseBody)
import           Yesod
import           Yesod.Static

data ParseError = ColumnNotFound !Text | CouldNotReadColumn !Text
    deriving (Show, Typeable)
instance Exception ParseError

-- | The type of visualization used to show some data.
data VisualizationType
  = LineChart
  | BarChart
  | PieChart
  deriving (Show,Enum,Eq)

-- | Default chart type to use for data.
instance Default VisualizationType where
  def = LineChart

-- | The format to export data generated by an analysis.
data ExportType
  = CsvData
  | XmlData
  | CsvDataGzip
  | XmlDataGzip
  deriving (Show,Read,Enum,Eq)

instance PathPiece ExportType where
  fromPathPiece "csv"     = Just CsvData
  fromPathPiece "xml"     = Just XmlData
  fromPathPiece "csv-gz"  = Just CsvDataGzip
  fromPathPiece "xml-gz"  = Just XmlDataGzip
  fromPathPiece _         = Nothing
  toPathPiece CsvData     = "csv"
  toPathPiece XmlData     = "xml"
  toPathPiece CsvDataGzip = "csv-gz"
  toPathPiece XmlDataGzip = "xml-gz"

-- | Default export type used if none is specified.
instance Default ExportType where
  def = CsvData

-- | A data point which can be rendered onto a chart of some kind.
data Data2D = D2D
  { _d2dLabel :: Text
  , _d2dValue :: Double
  , _d2dGroup :: Maybe Text
  } deriving (Show)

$(makeLenses ''Data2D)

instance ToJSON Data2D where
  toJSON (D2D label value group') =
    case group' of
      Nothing -> toJSON [toJSON label
                        ,toJSON value]
      Just group'' -> toJSON [toJSON label,toJSON value,toJSON group'']

-- | A 3D data point which can be rendered onto a 3D chart.
data Data3D = D3D
  { _dataX :: Int
  , _dataY :: Int
  , _dataZ :: Double
  } deriving (Show)

$(makeLenses ''Data3D)

instance ToJSON Data3D where
  toJSON (D3D x y z) =
    toJSON [toJSON x
           ,toJSON y
           ,toJSON z]

data DataPoint
  = DPM Text
  | DP2 Data2D
  | DP3 Data3D
  deriving (Show)

$(makePrisms ''DataPoint)

instance ToJSON DataPoint where
  toJSON (DP2 d) = object ["DP2" .= toJSON d]
  toJSON (DP3 d) = object ["DP3" .= toJSON d]
  toJSON (DPM t) = object ["DPM" .= toJSON t]

class ManagerReader m where
    askManager :: m Manager
class HasManager a where
    manager :: a -> Manager
instance (HasManager a, MonadUnsafeIO m, MonadThrow m, MonadBaseControl IO m, MonadIO m) => ManagerReader (HandlerT a m) where
    askManager = do
        x <- getYesod
        return $ manager x

class Default a => HasForm a where
    form :: RenderMessage site FormMessage => AForm (HandlerT site IO) a

staticFiles "static/"

-- | Yesod app type.
data App = App
  { appManager  :: !Manager
  , appTitle    :: !Text
  , appAnalysis :: !SomeAnalysis
  , appStatic   :: !Static
  , appStart    :: !UTCTime
  }

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Some analysis.
data SomeAnalysis = forall params. SomeAnalysis
  { analysisForm    :: !(AForm (HandlerT App IO) params)
  , analysisConduit :: !(IORef Int -> params -> Conduit ByteString (HandlerT App IO) DataPoint)
  , analysisDefaultParams :: !params
  }

-- | An imported data source.
data DataSource = DataSource
  { srcName      :: !Text
  , srcPath      :: !FilePath
  , srcTimestamp :: !UTCTime
  , srcUrl       :: !(Maybe String)
  } deriving (Show)

instance Eq DataSource where (==) = on (==) srcPath

instance Ord DataSource where compare = on compare srcTimestamp

sourceURL :: (MonadResource m, MonadBaseControl IO m, ManagerReader m)
          => String -> Source m ByteString
sourceURL url = do
    req <- liftIO $ parseUrl url
    m <- lift askManager
    res <- lift $ http req m
    (src, _) <- lift $ unwrapResumable $ responseBody res
    src

getSomeAnalysis
  :: (PersistEntity b, HasForm params) =>
     (params -> ConduitM b DataPoint (HandlerT App IO) ())
     -> SomeAnalysis
getSomeAnalysis userAnalysis = SomeAnalysis
    form
    (\countRef params ->
       fromBinary =$=
       CL.iterM (const (liftIO (modifyIORef' countRef (+1)))) =$=
       userAnalysis params)
    def
  where modifyIORef' :: IORef a -> (a -> a) -> IO ()
        modifyIORef' ref f = do
          x <- readIORef ref
          let x' = f x
          x' `seq` writeIORef ref x'

        fromBinary =
            csvIntoEntities (Proxy :: Proxy b) =$=
            CL.mapM (either (monadThrow . Ex) return)

getSomeAnalysisRaw
  :: HasForm params =>
     (params -> ConduitM ByteString DataPoint (HandlerT App IO) ())
     -> SomeAnalysis
getSomeAnalysisRaw userAnalysis = SomeAnalysis
    form
    (\countRef params ->
       CL.iterM (const (liftIO (modifyIORef' countRef (+1)))) =$=
       userAnalysis params)
    def
  where modifyIORef' :: IORef a -> (a -> a) -> IO ()
        modifyIORef' ref f = do
          x <- readIORef ref
          let x' = f x
          x' `seq` writeIORef ref x'

data Ex = Ex Text
  deriving (Data,Show,Typeable,Eq)
instance Exception Ex
