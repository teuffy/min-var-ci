{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Demo.Common where

import Yesod
import Demo.Helper.Class
import Network.HTTP.Conduit (Manager, newManager, def)
import Data.Conduit
import Data.Proxy

data App = App Manager SomeAnalysis

instance HasManager App where
    manager (App m _) = m

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler TypedContent
getHomeR = do
    App _ (SomeAnalysis _ aform userAnalysis) <- getYesod
    ((res, widget), enctype) <- runFormGet $ renderDivs aform
    case res of
        FormSuccess params -> respondSource "text/plain" $ userAnalysis params
        _ -> defaultLayout (do
            setTitle "RSI calculator"
            [whamlet|
                <form method=get action=@{HomeR}>
                    ^{widget}
                    <input type=submit value=Calculate>
            |]) >>= sendResponse

launch :: HasAnalysis params => Proxy params -> IO ()
launch params = do
    man <- newManager def
    warpEnv $ App man $ getSomeAnalysis params