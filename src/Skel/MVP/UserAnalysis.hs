{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.MVP.UserAnalysis (userAnalysis) where

import           Control.Monad
import qualified Data.Conduit.List as CL
import           Data.IORef.Lifted
import qualified Data.Packed.Vector as V
import           Data.Text (pack)
import           Numeric.LinearAlgebra
import           Skel.MVP.UserParameters hiding (Vector,(<.>))

-- | Stock price.
type Price = Double

-- | The top-level analysis which pulls stock entries from the
-- database, yields DataPrices values to 'mvpAnalysis' which in turn
-- yields 'DataPoint'.
userAnalysis :: MvpParams -> Conduit Security (YesodDB App) DataPoint
userAnalysis params =
  zipSecurities =$=
  mvpAnalysis params
  where zipSecurities =
          do prev <- newIORef Nothing
             awaitForever
                (\sec1 ->
                   do mp <- readIORef prev
                      case mp of
                        Nothing -> writeIORef prev (Just sec1)
                        Just sec2 -> do writeIORef prev Nothing
                                        yield (DatePrices d c1 c2)
                          where (Security _ d c1) = sec1
                                (Security _ _ c2) = sec2)

mvpAnalysis :: MvpParams -> Conduit DatePrices (YesodDB App) DataPoint
mvpAnalysis _ =
  do rows <- CL.consume
     case rows of
       [] -> return ()
       rows ->
         do let !matrix = fromLists (map toList rows)
                !rs = returns matrix
                !portfolio = minVariancePortfolio rs
            forM_ (zip [1..] (reverse (V.toList portfolio)))
                  (\(i,y) -> yield (DP2 (D2D (pack (show i)) y Nothing)))
  where toList !(DatePrices _ x y) = [x,y]

returns :: Matrix Price -> Matrix Price
returns = fromRows . returns' . toRows where
    returns' ps = map ret (zip ps (drop 1 ps))
    ret (ptt, pttp1) = (pttp1 / ptt) - 1

minVariancePortfolio :: Matrix Price -> Vector Price
minVariancePortfolio rets = tv / scalar (tvu <.> tv)
    where (_, cov) = meanCov rets
          vu = constant 1.0 (cols cov)
          tvu = constant 1.0 (rows cov)
          tv = inv cov <> vu
