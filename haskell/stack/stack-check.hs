#!/usr/bin/env stack
{- stack
  --resolver lts-18.28
  script
  --package rio
  --package text
  --package bytestring
  --package unliftio
  -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import RIO
import RIO.List as List
import RIO.List.Partial as List
import RIO.Directory
import UnliftIO.Temporary
import RIO.Process
import RIO.Text as T
import Data.Text.Encoding as T
import Data.Text.Encoding.Error as T
import Data.ByteString.Lazy as BL
import System.Environment (getArgs, getProgName)

type Args = [String]


execInfo_ :: FilePath -> Args -> RIO SimpleApp ()
execInfo_ cmd args = do
  logInfo $ display $ T.pack $ P.unwords (cmd : args)
  proc cmd args (runProcess . setStdin inherit) >>= \case
    ExitSuccess -> return ()
    exitCode -> do
      logError $ "Executed process returned with: " <> displayShow exitCode
      liftIO $ exitWith exitCode

execInfo :: FilePath -> Args -> RIO SimpleApp (Text, Text)
execInfo cmd args = do
  logInfo $ display $ T.pack $ P.unwords (cmd : args)
  proc cmd args (readProcess . setStdin inherit) >>= \case
    (ExitSuccess, bsOut, bsErr) -> pure (toText bsOut, toText bsErr)
    (exitCode, bsOut, bsErr) -> do
      logError $ "Executed process returned with: " <> displayShow exitCode
      logError $ "stdout: " <> display (toText bsOut)
      logError $ "stderr: " <> display (toText bsErr)
      liftIO $ exitWith exitCode
  where
    toText = T.decodeUtf8With strictDecode . BL.toStrict

main :: IO ()
main = do
  progName <- fromString <$> getProgName
  let usageMessage =
        "Missing package name, supply it as \n" <> "   $ " <> progName <>
        " $package-$version\n" <>
        "$version is optional\n" <>
        "By default it will be executed in the system temporary directory. " <>
        "Alternatively a directory can be supplied where package is unpacked and bulid:\n" <>
        "   $ " <>
        progName <>
        " $HOME/tmp' $package-$version\n"
  runSimpleApp $
    liftIO getArgs >>= \case
      [] -> logInfo usageMessage
      [packageNameVersion] ->
        withSystemTempDirectory "stack-check" $ \dir ->
          checkPackage dir packageNameVersion
      [dir, packageNameVersion] -> do
        createDirectoryIfMissing True dir
        checkPackage dir packageNameVersion
      args ->
        logError $
        "Invalid arguments: " <> displayShow args <> "\n" <> usageMessage
  where
    checkPackage dir packageNameVersion =
      withCurrentDirectory dir $ do
        execInfo_ "stack" ["update"]
        (txtOut, txtErr) <- execInfo "stack" ["unpack", packageNameVersion]
        logDebug $ "stdout: " <> display txtOut
        logDebug $ "stderr: " <> display txtErr
        let fp = T.unpack $ T.unwords $ List.drop 5 $ T.words txtErr
        logInfo $ "Running in: " <> fromString fp
        withCurrentDirectory fp $ do
          execInfo_ "stack" ["init", "--resolver", "nightly"]
          execInfo_
            "stack"
            [ "build"
            , "--resolver"
            , "nightly"
            , "--haddock"
            , "--test"
            , "--bench"
            , "--no-run-benchmarks"
            ]
