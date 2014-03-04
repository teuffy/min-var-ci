-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.MVP.UserAnalysis (userAnalysis) where

import Skel.MVP.UserParameters

mvpAnalysis :: MvpParams -> Conduit DatePrices (YesodDB App) DataPoint
mvpAnalysis (MvpParams size alpha bars) =
  do awaitForever (const (return ()))
     yield (DPM "Hello?")

userAnalysis :: MvpParams -> Conduit Security (YesodDB App) DataPoint
userAnalysis params =
  (do liftIO (appendFile "/tmp/alog" "userAnalysis\n")
      awaitForever (\sec -> do liftIO (appendFile "/tmp/alog" (show sec ++ "\n"))
                               today <- liftIO getCurrentTime
                               yield (DatePrices (utctDay today) 1 2))) =$=
  mvpAnalysis params
