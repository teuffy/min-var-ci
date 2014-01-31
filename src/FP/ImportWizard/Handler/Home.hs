{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS -Wall -Werror -funbox-strict-fields #-}

module FP.ImportWizard.Handler.Home where

import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as Path
import           Network.HTTP.Types.URI       (urlEncode)
import           Safe                         (readMay)
import           System.Exit                  (ExitCode (..))
import           System.Process               (readProcessWithExitCode)

import           FP.ImportWizard.Generate
import           FP.ImportWizard.Import
import           FP.ImportWizard.SourceWizard
import           FP.ImportWizard.Temp
import           FP.ImportWizard.Types
import           FP.ImportWizard.Wizard

getHomeR :: Handler Html
getHomeR = renderHome Nothing

renderHome :: Maybe IWData -> Handler Html
renderHome maybeData = defaultLayout $ do
    setTitle "Import wizard: Data sources"
    [whamlet|
        $case maybeData
            $of Nothing
                <p>You do not have any data sources
                <form method=get action=@{AddSourceR}>
                    <input type=submit value="Add a data source">
            $of Just data_
                <form method=post action=@{CreateProjectR} enctype=#{Multipart}>
                    <table border>
                        <tr>
                            <th>Name
                            <th>Format
                            <th>Action
                        <tr>
                            <td>#{iwfdName $ iwdFormat data_}
                            <td>#{formatTitle $ iwfdFormat $ iwdFormat data_}
                            <td><input type=submit value="Remove" onclick="alert('Not supported in demo!'); return false">
                    <input type=submit value="Add another data source" onclick="alert('Not supported in demo!'); return false">
                    <br><br>
                    <input type=hidden name=data value=#{show data_}>
                    <input type=submit value="Create project in IDE">
    |]

getAddSourceR :: Handler Html
getAddSourceR = do
    wizardResult <- runWizard config
    case wizardResult of
        WizardCancelled -> redirect HomeR
        WizardSuccess html -> return html
        WizardFinished data_ -> renderHome $ Just data_
  where
    config = (wizardConfig defaultData iwPageHandler)
        {   wcGetPageTitle = return . Just . iwPageTitle }
        -- EKB FIXME move next to IWData def
    defaultData = defaultIwData

postAddSourceR :: Handler Html
postAddSourceR = getAddSourceR

postCreateProjectR :: Handler Html
postCreateProjectR = do
    (postFields, _) <- runRequestBody
    let data_ = fromMaybe (error "Invalid/missing data") $ (Safe.readMay . Text.unpack) =<< lookup "data" postFields
    (tempToken, tempPath) <- newTempToken
    liftIO $ do
        FS.createTree tempPath
        runSystem "cp" ["-r", "skel/.", Path.encodeString tempPath]
        runSystem "rm" ["-rf", Path.encodeString $ tempPath </> "src" </> "Skel"]
        generatedCode <- generateCode data_
        forM_ generatedCode $ \(path, code) ->
            FS.writeFile (tempPath </> "src" </> path) $ Text.encodeUtf8 code
        let gitArgs =
                [   "--git-dir=" ++ Path.encodeString (tempPath </> ".git")
                ,   "--work-tree=" ++ Path.encodeString tempPath
                ,   "-c", "user.email=noreply@fpcomplete.com" -- EKB TODO better address?
                ,   "-c", "user.name=FP Complete Import Wizard"]
        runSystem "git" $ gitArgs ++ ["add", "--all"]
        runSystem "git" $ gitArgs ++ ["commit", "-m", "Generated code"] -- EKB TODO better message?
        runSystem "git" $ gitArgs ++ ["update-server-info"]
    urlRender <- getUrlRender
    redirect $  "https://www.fpcomplete.com/ide?title="
            ++  urlEncodeText ("Data analysis demo: " ++ iwfdName (iwdFormat data_))
            ++ "&git=" ++  urlEncodeText (urlRender $ GitR tempToken [])

runSystem :: (MonadIO m) => String -> [String] -> m ()
runSystem cmd args = liftIO $ do
    (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure f -> error $ "runSystem: " ++ cmd ++ " " ++ Text.unpack (show args)
             ++ " failed with exit code " ++ Text.unpack (show f) ++ ": " ++ stderr

urlEncodeText :: Text -> Text
urlEncodeText = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8

getGitR :: TempToken -> [Text] -> Handler ()
getGitR tempToken pathPieces = do
    gitPath <- (</> ".git/") <$> getTempTokenPath tempToken
    let reqPath = Path.collapse $ gitPath </> Path.concat (map Path.fromText pathPieces)
    -- EKB FIXME not sure how secure this is (how dependable is Path.collapse?).  Also, needs testing
    when (Path.commonPrefix [gitPath, reqPath] /= gitPath) $
        error "Bad git path"
    sendFile "application/octet-stream" (Path.encodeString reqPath)
