{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Check to see if two binary files differ.

module Data.Conduit.Equal where

import           Control.Monad.Fix
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CL

-- | Are the sources equal?
sourcesEqual :: (Eq b, Monad m) => Source m b -> Source m b -> m (Maybe Bool)
sourcesEqual x y = CL.zip x y $= allEqual $$ CL.head
  where
    allEqual =
      fix (\loop ->
             do mchunk <- await
                case mchunk of
                  Nothing -> yield True
                  Just (i,j) | i == j -> loop
                             | otherwise -> yield False)
