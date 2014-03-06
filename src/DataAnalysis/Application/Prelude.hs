{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  ( runAnalysisAppDb
  )
  where

import           Control.Monad.Logger
-- import           Control.Monad.Reader (ReaderT)
-- import           Data.ByteString (ByteString)
import           Data.CSV.Conduit.Persist
import           Data.Conduit
import qualified Data.Conduit.List as CL
-- import           Data.Default
import           Data.IORef
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time
import           DataAnalysis.Application.Dispatch ()
import           DataAnalysis.Application.Types
import           Database.Persist.Sqlite
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Skel.MVP.UserModel
import           Skel.MVP.UserParameters
import           System.IO
-- import           Yesod
import           Yesod.Static

-- | Run the analysis web app with a conduit from the database.
runAnalysisAppDb
  ::
     Text
     -> SqlPersistM a
     -> (MvpParams
         -> Conduit Security (YesodDB App) DataPoint)
     -> IO ()
runAnalysisAppDb title migrate analysis = do
  s <- static "static"
  man <- newManager tlsManagerSettings
  now <- getCurrentTime
  pool <- makePool migrate
  from <- fmap utctDay getCurrentTime
  to <- fmap utctDay getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysisDb from to analysis)
         s
         now
         pool)

  where getSomeAnalysisDb from to userAnalysis = SomeAnalysis
            form
            (Left ((\countRef params ->
                      selectSource ([SecurityDate >=. d | Just d <- [paramsFrom params]] ++
                                    [SecurityDate <=. d | Just d <- [paramsTo params]])
                                   [Asc SecurityDate] =$=
                      CL.iterM (const (liftIO (modifyIORef' countRef (+1)))) =$=
                      CL.map entityVal =$=
                      userAnalysis params)))
            (Just (\name source ->
                     do xs <- liftIO (runResourceT (source $= fromBinary $$ CL.consume))
                        mapM_ (insert . convert name) xs))
            (MvpParams Nothing Nothing)
            True
          where modifyIORef' :: IORef a -> (a -> a) -> IO ()
                modifyIORef' ref f = do
                  x <- readIORef ref
                  let x' = f x
                  x' `seq` writeIORef ref x'
                fromBinary =
                    csvIntoEntities (Proxy :: Proxy Stock) =$=
                    CL.mapM (either (monadThrow . Ex) return)
                convert :: Text -> Stock -> Security
                convert name (Stock day _open _high _low close _vol _adj) =
                  Security name day close

-- | Make a pool and migrate it.
makePool :: SqlPersistM a -> IO ConnectionPool
makePool migrate =
  do (fp,h) <- openTempFile "/tmp/" "finance.db"
     hClose h
     pool <- runNoLoggingT (createSqlitePool (T.pack fp) 1)
     day <- fmap utctDay getCurrentTime
     runSqlPersistMPool migrate pool
     return pool
