{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Datatypes for analysis.

module Types
  (Parameters(..)
  ,Listen
  ,ListenGeneric(..)
  ,ListenId)
  where

import Control.Lens.TH
import Data.Default        (Default (..))
import Data.Text           (Text)
import Data.Time
import Data.Typeable
import Database.Persist.TH (mkPersist, persistLowerCase, share, sqlSettings)
#ifdef FPHC
import Database.Persist.Sql
#endif

-- | Parameters to the analysis app.
data Parameters = Parameters
  { paramStart :: Maybe Double
  , paramEnd   :: Maybe Double
  } deriving (Show,Typeable)

-- | Default values for the parameters.
instance Default Parameters where
  def = Parameters
    { paramStart = Just 50
    , paramEnd   = Just 80
    }

-- Using persistent's SQL TH for now. I'm not sure what the end result
-- will look like.
share [mkPersist sqlSettings] [persistLowerCase|
Listen
  timestamp UTCTime
  title Text
  artist Text
  deriving Show
|]

#ifdef FPHC
deriving instance Typeable SqlBackend
#endif
deriving instance Typeable1 ListenGeneric

-- Just because I don't know how to generate these automatically.
makeLensesFor [("listenTimestamp","_listenTimestamp")
              ,("listenTitle","_listenTitle")
              ,("listenArtist","_listenArtist")]
              ''ListenGeneric
