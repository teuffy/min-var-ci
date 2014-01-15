{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Werror #-}

module FP.ImportWizard.Wizard where

import           BasicPrelude
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Yesod                      hiding (get)

runWizard
    ::  (WizardData data_, MonadHandler handler, WizardPage page)
    => WizardConfig data_ page handler result -> handler (WizardResult result)
runWizard config@WizardConfig{..} = do

    -- EKB TODO version of handleWizard that takes body as argument? or can runRequestBody be done multiple times?
    (postFields, _) <- runRequestBody

    let wps = case readMay =<< lookup (_stateFieldName config) postFields of
            Just wps'@WizardPageState{wpsSubmittedPage = Just _} -> wps'
            _   ->  WizardPageState
                {   wpsSubmittedPage    =   Nothing
                ,   wpsPageStack        =   [wcInitialPage]
                ,   wpsData             =   wcDefaultData   }
        state = WizardState
                {   wsPostFields    =   postFields
                ,   wsPageState     =   wps         }

    (result, _) <- _runWizardT config state _runPageHandler
    return result

_runWizardT :: c -> s -> StateT s (ReaderT c m) a -> m (a, s)
_runWizardT config state action =
    runReaderT (runStateT action state) config

_getConfig
    ::  (MonadHandler handler)
    =>  WizardT data_ page handler result (WizardConfig data_ page handler result)
_getConfig = ask

_getState
    ::  (MonadHandler handler)
    =>  WizardT data_ page handler result (WizardState data_ page)
_getState = get

_putState
    ::  (MonadHandler handler)
    =>  WizardState data_ page -> WizardT data_ page handler result ()
_putState = put

liftWizardHandler
    ::  (MonadHandler handler)
    =>  handler a -> WizardT data_ page handler result a
liftWizardHandler = lift . lift

runWizardForm
    ::  (   RenderMessage (HandlerSite handler) FormMessage
        ,   MonadResource handler, MonadHandler handler, WizardData data_, WizardPage page)
    =>  (Html -> MForm handler (FormResult b, xml))
    ->  WizardT data_ page handler result ((WizardFormResult b, xml), Enctype)
runWizardForm form = do
    WizardConfig{..} <- _getConfig
    state <- _getState
    maybeNav <- getNavigationSubmitted
    case maybeNav of
        Just nav -> do
            ((result, widget), enctype) <- liftWizardHandler $ runFormPost form
            mustValidate <- wcNavigationValidateForm (_topPage state) nav
            if mustValidate
                then return ((WizardFormNext result, widget), enctype)
                else case result of
                    FormSuccess _ -> return ((WizardFormSave result, widget), enctype)
                    _ -> do
                        _ <- getMessage -- just to clear the message
                        return ((WizardFormDiscard, widget), enctype)
        Nothing -> do
            (widget, enctype) <- liftWizardHandler $ generateFormPost form
            return ((WizardFormSave FormMissing, widget), enctype)

-- EKB TODO not super happy with this approach, ends up needing too much repetition
data WizardFormResult b
    = WizardFormDiscard
    | WizardFormSave (FormResult b)
    | WizardFormNext (FormResult b)

getWizardPageTitle
    ::  (MonadHandler handler, WizardData data_, WizardPage page)
    =>  WizardT data_ page handler result (Maybe Html)
getWizardPageTitle = do
    WizardConfig{..} <- _getConfig
    page <- getWizardPage
    wcGetPageTitle page

getWizardData :: (MonadHandler handler) => WizardT data_ page handler result data_
getWizardData = liftM (wpsData . wsPageState) _getState

putWizardData :: (MonadHandler handler) => data_ -> WizardT data_ page handler result ()
putWizardData data_ = do
    state@WizardState{..} <- _getState
    _putState state{wsPageState = wsPageState{wpsData = data_}}

getWizardPage
    ::  (MonadHandler handler, WizardData data_, WizardPage page)
    =>  WizardT data_ page handler result page
getWizardPage = liftM _topPage _getState

_topPage :: (WizardData data_, WizardPage page) => WizardState data_ page -> page
_topPage WizardState{wsPageState = WizardPageState{..}} =
    case wpsPageStack of
        []      ->  minBound
        (p:_)   ->  p

getIsPageSubmitted
    :: (MonadHandler handler, WizardData data_, WizardPage page)
    => WizardT data_ page handler result Bool
getIsPageSubmitted = liftM _isTopPageSubmitted _getState

_isTopPageSubmitted :: (WizardPage page, WizardData data_) => WizardState data_ page -> Bool
_isTopPageSubmitted state@WizardState{wsPageState = WizardPageState{..}} =
    wpsSubmittedPage == Just (_topPage state)

type WizardData data_ = (Read data_, Show data_)

data WizardConfig data_ page handler result = WizardConfig
    {   wcDefaultData           ::  data_
    ,   wcInitialPage           ::  page
    ,   wcPageHandler           ::  WizardPageHandler data_ page handler result
    ,   wcGetPageTitle
            ::  (MonadHandler handler, WizardPage page)
            =>  page -> WizardT data_ page handler result (Maybe Html)
    ,   wcGetNextPage
            ::  (MonadHandler handler)
            =>  page -> WizardT data_ page handler result (Maybe page)
    ,   wcGetShowNavigation
            ::  (MonadHandler handler)
            =>  page -> WizardNavigation -> WizardT data_ page handler result Bool
    ,   wcGetNavigationLabel
            ::  (MonadHandler handler)
            =>  page -> WizardNavigation -> WizardT data_ page handler result Html
        -- EKB TODO rename?
    ,   wcOnNavigation
            ::  (WizardPage page, MonadHandler handler)
            =>  page -> WizardNavigation -> WizardT data_ page handler result (WizardResult result)
        -- EKB TODO rename?
    ,   wcNavigationValidateForm
            ::  (WizardPage page, MonadHandler handler)
            =>  page -> WizardNavigation -> WizardT data_ page handler result Bool
    ,   wcFieldPrefix           ::  Text   }

wizardConfig
    :: (WizardData data_, WizardPage page)
    => data_
    -> WizardPageHandler data_ page handler result
    -> WizardConfig data_ page handler result
wizardConfig defaultData pageHandler = WizardConfig
    {   wcDefaultData               =   defaultData
    ,   wcInitialPage               =   minBound
    ,   wcPageHandler               =   pageHandler
    ,   wcGetPageTitle              =   \_ -> return Nothing
    ,   wcGetNextPage               =   defaultGetNextPage
    ,   wcGetShowNavigation         =   defaultGetWizardShowNavigation
    ,   wcGetNavigationLabel        =   defaultGetWizardNavigationLabel
    ,   wcOnNavigation              =   defaultWizardOnNavigation
    ,   wcNavigationValidateForm    =   defaultWizardNavigationValidateForm
    ,   wcFieldPrefix               =   "__Yesod.Wizard" }

type WizardPageHandler data_ page handler result
    =   (WizardPage page, MonadHandler handler, WizardData data_)
    =>  data_ -> page -> WizardT data_ page handler result (WizardResult result)

defaultWizardOnNavigation
    ::  (WizardPage page, MonadHandler handler, WizardData data_)
    =>  page
    ->  WizardNavigation
    ->  WizardT data_ page handler result (WizardResult result)

defaultWizardOnNavigation _ WizardCancel =
    return WizardCancelled

defaultWizardOnNavigation _ WizardBack =
    navigateWizardBack

defaultWizardOnNavigation _ WizardNext =
    navigateWizardNext

defaultGetNextPage
    ::  (MonadHandler handler, WizardPage page)
    =>  page -> WizardT data_ page handler result (Maybe page)
defaultGetNextPage page =
    return $ if page == maxBound
        then Nothing
        else Just $ succ page

defaultGetWizardShowNavigation
    ::  (MonadHandler handler)
    =>  page -> WizardNavigation -> WizardT data_ page handler result Bool
defaultGetWizardShowNavigation _ WizardCancel = return True
defaultGetWizardShowNavigation _ WizardBack = do
    WizardState{wsPageState = WizardPageState{..}} <- _getState
    return $ case wpsPageStack of
        (_ : _ : _) ->  True
        _           ->  False
defaultGetWizardShowNavigation _ WizardNext = return True

-- EKB TODO use Yesod messages for l10n
defaultGetWizardNavigationLabel
    ::  (MonadHandler handler, WizardPage page)
    =>  page -> WizardNavigation -> WizardT data_ page handler result Html
defaultGetWizardNavigationLabel _ WizardCancel  =   return "Cancel"
defaultGetWizardNavigationLabel _ WizardBack    =   return "Back"
defaultGetWizardNavigationLabel page WizardNext = do
    WizardConfig{..} <- _getConfig
    maybeNextPage <- wcGetNextPage page
    maybeNextPageTitle <- case maybeNextPage of
        Nothing -> return Nothing
        Just nextPage -> wcGetPageTitle nextPage
    return $ "Next" ++ maybe "" (": " ++) maybeNextPageTitle

defaultWizardNavigationValidateForm
    ::  (MonadHandler handler, WizardPage page)
    =>  page -> WizardNavigation -> WizardT data_ page handler result Bool
defaultWizardNavigationValidateForm _ WizardCancel  = return False
defaultWizardNavigationValidateForm _ WizardBack    = return False
defaultWizardNavigationValidateForm _ WizardNext    = return True

_runPageHandler
    ::  (MonadHandler handler, WizardPage page, WizardData data_)
    =>  WizardT data_ page handler result (WizardResult result)
_runPageHandler = do
    WizardConfig{..} <- _getConfig
    data_ <- getWizardData
    page <- getWizardPage
    wcPageHandler data_ page

navigateWizardBack
    ::  (WizardPage page, WizardData data_, MonadHandler handler)
    =>  WizardT data_ page handler result (WizardResult result)
navigateWizardBack =
    _navigateModifyPageStack $ \pageStack ->
        case pageStack of
            []      ->  [minBound]
            [p]     ->  [p]
            (_:ps)  ->  ps

navigateWizardNext
    ::  (WizardPage page, WizardData data_, MonadHandler handler)
    =>  WizardT data_ page handler result (WizardResult result)
navigateWizardNext = do
    WizardConfig{..} <- _getConfig
    page <- getWizardPage
    maybeNextPage <- wcGetNextPage page
    case maybeNextPage of
        Just nextPage -> navigateWizardForward nextPage
        Nothing -> _runPageHandler

navigateWizardForward
    :: (MonadHandler handler, WizardData data_, WizardPage page)
    => page -> WizardT data_ page handler result (WizardResult result)
navigateWizardForward nextPage = _navigateModifyPageStack (nextPage :)

continueWizard
    ::  (MonadHandler handler, WizardPage page, WizardData data_)
    =>  data_ -> WizardT data_ page handler result (WizardResult result)
continueWizard newData = do
    -- EKB TODO probably all the other navigate* functions should also take newData.
    --      must be careful with _runPageHandler then (maybe it should also take newData)
    putWizardData newData
    maybeNav <- getNavigationSubmitted
    case maybeNav of
        Just nav    -> navigateWizardWith nav
        Nothing     -> _runPageHandler

getNavigationSubmitted
    ::  (MonadHandler handler, WizardPage page, WizardData data_)
    =>  WizardT data_ page handler result (Maybe WizardNavigation)
getNavigationSubmitted = do
    config  <-  _getConfig
    state   <-  _getState
    if _isTopPageSubmitted state
        then do
            let navs = catMaybes $ flip map [minBound..maxBound] $ \nav ->
                    if _isNavigationSubmitted config state nav
                        then    Just nav
                        else    Nothing
            return $ case navs of
                (nav:_) ->  Just nav
                _       ->  Nothing
        else
            return Nothing

_isNavigationSubmitted
    :: (WizardData data_, WizardPage page)
    => WizardConfig data_ page handler result
    -> WizardState data_ page
    -> WizardNavigation
    -> Bool
_isNavigationSubmitted
        config
        WizardState{wsPostFields, wsPageState = WizardPageState{..}}
        nav =
    isJust $ lookup (_navigationFieldName config nav) wsPostFields

navigateWizardWith
    ::  (MonadHandler handler, WizardPage page, WizardData data_)
    =>  WizardNavigation -> WizardT data_ page handler result (WizardResult result)
navigateWizardWith nav = do
    WizardConfig{..}    <-  _getConfig
    page                <-  getWizardPage
    wcOnNavigation page nav

_navigateModifyPageStack
    :: (MonadHandler handler, WizardData data_, WizardPage page)
    => ([page] -> [page]) -> WizardT data_ page handler result (WizardResult result)
_navigateModifyPageStack f = do
    WizardConfig{..} <- _getConfig
    state@WizardState{wsPageState = pageState@WizardPageState{..}} <- _getState
    _putState state{wsPageState = pageState{wpsPageStack = f wpsPageStack}}
    _runPageHandler

getWizardNavigationsWidget
    :: (MonadHandler handler, WizardPage page, WizardData data_)
    => WizardT data_ page handler result Html
getWizardNavigationsWidget = do
    -- EKB TODO improve
    ws <- getWizardStateWidget
    w1 <- getWizardNavigationWidget WizardCancel
    w2 <- getWizardNavigationWidget WizardBack
    w3 <- getWizardNavigationWidget WizardNext
    return (ws >> w1 >> w2 >> w3)

getWizardStateWidget
    ::  (MonadHandler handler, WizardData data_, WizardPage page)
    =>  WizardT data_ page handler result Html
getWizardStateWidget = do
    config <- _getConfig
    state@WizardState{..} <- _getState
    let stateWithTopPage = wsPageState{wpsSubmittedPage = Just (_topPage state)}
    return $ [shamlet|
        <input type="hidden" name=#{_stateFieldName config} value=#{show stateWithTopPage}> |]

getWizardNavigationWidget
    :: (MonadHandler handler, WizardPage page, WizardData data_)
    => WizardNavigation -> WizardT data_ page handler result Html
getWizardNavigationWidget nav = do
    config@WizardConfig{..} <- _getConfig
    page <- getWizardPage
    showNav <- wcGetShowNavigation page nav
    if showNav
        then do
            label <- wcGetNavigationLabel page nav
            return $ [shamlet|
                <input type=submit name=#{_navigationFieldName config nav} value=#{label}> |]
        else
            return (""::Html)

_stateFieldName :: WizardConfig data_ page handler result -> Text
_stateFieldName WizardConfig{..} = wcFieldPrefix ++ "__state__"

_navigationFieldName :: WizardConfig data_ page handler result -> WizardNavigation -> Text
_navigationFieldName WizardConfig{..} nav = wcFieldPrefix ++ "__nav_" ++ show nav ++ "__"

type WizardPage page = (Read page, Show page, Eq page, Enum page, Bounded page)

type WizardT data_ page handler result a = StateT
    (WizardState data_ page)
    (ReaderT (WizardConfig data_ page handler result) handler)
    a

data WizardState data_ page = WizardState
    {   wsPostFields :: [(Text, Text)]
    ,   wsPageState  :: WizardPageState page data_ }

data WizardPageState page data_ = WizardPageState
    {   wpsSubmittedPage :: Maybe page
    ,   wpsPageStack     :: [page]
    ,   wpsData          :: data_ }
    deriving (Read, Show)

data WizardNavigation
    =   WizardCancel
    |   WizardBack
    |   WizardNext
    deriving (Show, Enum, Bounded)

data WizardResult result
    =   WizardCancelled
    |   WizardSuccess result
