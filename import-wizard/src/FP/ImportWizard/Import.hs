{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Werror #-}

module FP.ImportWizard.Import
    ( module FP.ImportWizard.Import
    , module X
    ) where

import           BasicPrelude               as X hiding (delete, deleteBy,
                                                  insert, insertBy)
import           Data.Default
import qualified GHC.IO
import           Language.Haskell.TH
import           Yesod                      as X
import           Yesod.Default.Util
import           Yesod.Form.Jquery          as X (urlJqueryJs)

import           FP.ImportWizard.Foundation as X

widgetFile :: GHC.IO.FilePath -> ExpQ
widgetFile = widgetFileReload def
