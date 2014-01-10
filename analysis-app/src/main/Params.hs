{-# LANGUAGE OverloadedStrings #-}

-- | Parse the parameters from a formlet

module Params where

import Control.Applicative
import DataAnalysis.Application.Types
import DataAnalysis.Application.Foundation ()
import Types
import Yesod

params :: Html -> MForm (HandlerT GenericApp IO)
                        (FormResult Parameters,WidgetT GenericApp IO ())
params = renderDivs formlet
  where formlet =
          Parameters <$> aopt doubleField "Start" Nothing
                     <*> aopt doubleField "End"   Nothing
