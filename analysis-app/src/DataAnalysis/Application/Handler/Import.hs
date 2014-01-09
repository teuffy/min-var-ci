{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Import where

import Data.Default
import Yesod
import Yesod.Default.Util
import Data.Conduit
import Data.Conduit.Binary

import DataAnalysis.Application.Foundation

getImportR :: Handler Html
getImportR = do
    (formWidget, formEncType) <- generateFormPost uploadForm
    currentRoute <- getCurrentRoute
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileReload def "import")

postImportR :: Handler Html
postImportR = do
  ((result, _), _) <- runFormPost uploadForm
  case result of
    FormSuccess fi -> do
      app@App{appParser=parser} <- getYesod
      fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
      liftIO (parser fileBytes) >>= \x ->
        case x of
          Nothing -> error "Unable to parse CSV"
          Just source -> do
            i <- addSource app source
            redirect (ReviewR i)
    _ -> return ()
  redirect ImportR

uploadForm = renderDivs $ fileAFormReq "source"
