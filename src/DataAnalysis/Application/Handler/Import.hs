{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module DataAnalysis.Application.Handler.Import where

import Data.Default
import Data.Text (Text)
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

-- | Show the import form.
getImportR :: Handler Html
getImportR = getImport Nothing

-- | Add the given file as a data source and redirect to the new
-- source.
postImportR :: Handler Html
postImportR = do
  ((result, _), _) <- runFormPost uploadForm
  case result of
    FormSuccess fi -> do
      addSource fi
      redirect ReviewR
    _ -> do ((result', _), _) <- runFormPost urlForm
            case result' of
              FormSuccess url -> do
                mi <- addUrlSource url
                case mi of
                  Just i -> redirect ReviewR
                  Nothing -> getImport (Just "Unable to download from URL.")
              _ -> redirect ImportR

-- | Import form, takes maybe an error message.
getImport :: Maybe Text -> Handler Html
getImport merror = do
  (uploadFormWidget, uploadFormEncType) <- generateFormPost uploadForm
  (urlFormWidget, urlFormEncType) <- generateFormPost urlForm
  currentRoute <- getCurrentRoute
  defaultLayout $ do
      setTitle "File Processor"
      $(widgetFileReload def "import")

-- | Upload form.
uploadForm = renderDivs $ fileAFormReq "source"

-- | URL form.
urlForm = renderDivs $ areq textField "Address" Nothing
