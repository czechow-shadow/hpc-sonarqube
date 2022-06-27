-- |
-- Module:     Trace.Hpc.SonarQube.Main
-- Copyright:  (c) 2022 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Main function for @hpc-sonarqube@.
--
module Trace.Hpc.SonarQube.Main (defaultMain) where

-- base
import Control.Exception           (throwIO)
import System.Environment          (getArgs)

-- Internal
import Trace.Hpc.SonarQube.Exception
import Trace.Hpc.SonarQube.Options
import Trace.Hpc.SonarQube.Report

-- | The main function for @hpc-sonarqube@ executable.
defaultMain :: IO ()
defaultMain = withBriefUsageOnError (getArgs >>= go)
  where
    go args =
      case parseOptions args of
        Right opts | optShowHelp opts    -> printHelp
                   | optShowVersion opts -> printVersion
                   | optShowNumeric opts -> putStrLn versionString
                   | otherwise           -> opt2rpt opts >>= genReport
        Left errs -> throwIO (InvalidArgs errs)
