{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Demo.Common where

import Data.Default
import Data.Proxy
import Demo.Helper.Class
import Yesod

#ifdef FPHC
import Network.HTTP.Conduit (Manager, newManager, def)
defaultManagerSettings = def
#else
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
#endif

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
                <form method=get action=@{HomeR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit value=Calculate>
            |]) >>= sendResponse

launch :: HasAnalysis params => Proxy params -> IO ()
launch params = do
    man <- newManager defaultManagerSettings
    warpEnv $ App man $ getSomeAnalysis params
