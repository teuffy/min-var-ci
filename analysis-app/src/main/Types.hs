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

-- | Parameters to the analysis app.
data Parameters = Parameters
  { paramStart :: Maybe Int
  , paramEnd   :: Maybe Int
  } deriving (Show,Typeable)

-- | Default values for the parameters.
instance Default Parameters where
  def = Parameters
    { paramStart = Nothing
    , paramEnd   = Nothing
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

deriving instance Typeable1 ListenGeneric

-- Just because I don't know how to generate these automatically.
makeLensesFor [("listenTimestamp","_listenTimestamp")
              ,("listenTitle","_listenTitle")
              ,("listenArtist","_listenArtist")]
              ''ListenGeneric
