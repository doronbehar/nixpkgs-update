{-# LANGUAGE OverloadedStrings #-}

module Git
  ( cleanAndResetTo
  , cleanup
  , fetchIfStale
  , fetch
  , push
  , checkoutAtMergeBase
  , checkAutoUpdateBranchDoesntExist
  , commit
  , headHash
  , deleteBranchEverywhere
  ) where

import OurPrelude

import Control.Concurrent

import Control.Exception hiding (catch, throw)
import qualified Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory (getHomeDirectory, getModificationTime)
import System.Exit
import Utils (Options(..), UpdateEnv(..), branchName, overwriteErrorT)

clean :: ProcessConfig () () ()
clean = silently "git clean -fdx"

checkout :: Text -> Text -> ProcessConfig () () ()
checkout branch target =
  silently $ proc "git" ["checkout", "-B", T.unpack branch, T.unpack target]

reset :: Text -> ProcessConfig () () ()
reset target = silently $ proc "git" ["reset", "--hard", T.unpack target]

delete :: Text -> ProcessConfig () () ()
delete branch = silently $ proc "git" ["branch", "-D", T.unpack branch]

deleteOrigin :: Text -> ProcessConfig () () ()
deleteOrigin branch =
  silently $ proc "git" ["push", "origin", T.unpack (":" <> branch)]

cleanAndResetTo :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do runProcessNoIndexIssue_ $ silently "git reset --hard"
         runProcessNoIndexIssue_ clean
         runProcessNoIndexIssue_ $ checkout branch target
         runProcessNoIndexIssue_ $ reset target
         runProcessNoIndexIssue_ clean

cleanup :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
cleanup bName = do
  liftIO $ T.putStrLn ("Cleaning up " <> bName)
  cleanAndResetTo "master"
  runProcessNoIndexIssue_ (delete bName) `catch`
    const (liftIO $ T.putStrLn ("Couldn't delete " <> bName))

staleFetchHead :: MonadIO m => m Bool
staleFetchHead =
  liftIO $ do
    home <- getHomeDirectory
    let fetchHead = home <> "/.cache/nixpkgs/.git/FETCH_HEAD"
    oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
    fetchedLast <- getModificationTime fetchHead
    return (fetchedLast < oneHourAgo)

fetchIfStale :: Members '[ Lift IO, Error Text] r => Sem r ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: Members '[ Lift IO, Error Text] r => Sem r ()
fetch =
  runProcessNoIndexIssue_ $
  silently "git fetch -q --prune --multiple upstream origin"

push :: Members '[ Lift IO, Error Text] r => UpdateEnv -> Sem r ()
push updateEnv =
  runProcessNoIndexIssue_
    (proc
       "git"
       ([ "push"
        , "--force"
        , "--set-upstream"
        , "origin"
        , T.unpack (branchName updateEnv)
        ] ++
        ["--dry-run" | dryRun (options updateEnv)]))

checkoutAtMergeBase :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
checkoutAtMergeBase bName = do
  base <-
    readProcessInterleavedNoIndexIssue_
      "git merge-base upstream/master upstream/staging" <&>
    T.strip
  runProcessNoIndexIssue_ (checkout bName base)

checkAutoUpdateBranchDoesntExist ::
     Members '[ Lift IO, Error Text] r => Text -> Sem r ()
checkAutoUpdateBranchDoesntExist pName = do
  remoteBranches <-
    readProcessInterleavedNoIndexIssue_ "git branch --remote" <&>
    (T.lines >>> fmap T.strip)
  throw "Update branch already on origin. "
  when
    (("origin/auto-update/" <> pName) `elem` remoteBranches)
    (throw "Update branch already on origin. ")

commit :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
commit ref =
  runProcessNoIndexIssue_ (proc "git" ["commit", "-am", T.unpack ref])

headHash :: Members '[ Lift IO, Error Text] r => Sem r Text
headHash = readProcessInterleavedNoIndexIssue_ "git rev-parse HEAD"

deleteBranchEverywhere :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
deleteBranchEverywhere bName = do
  runProcessNoIndexIssue_ (delete bName) `catch`
    (const $ liftIO $ T.putStrLn $ ("Couldn't delete " <> bName))
  runProcessNoIndexIssue_ (deleteOrigin bName) `catch`
    (const $ liftIO $ T.putStrLn $ "Couldn't delete " <> bName <> " on origin")

runProcessNoIndexIssue_ ::
     Members '[ Lift IO, Error Text] r => ProcessConfig () () () -> Sem r ()
runProcessNoIndexIssue_ config = tryIOTextET go
  where
    go = do
      (code, out, e) <- readProcess config
      case code of
        ExitFailure 128
          | "index.lock" `BS.isInfixOf` BSL.toStrict e -> do
            threadDelay 100000
            go
        ExitSuccess -> return ()
        ExitFailure _ ->
          Control.Exception.throw $ ExitCodeException code config out e

readProcessInterleavedNoIndexIssue_ ::
     Members '[ Lift IO, Error Text] r => ProcessConfig () () () -> Sem r Text
readProcessInterleavedNoIndexIssue_ config = tryIOTextET go
  where
    go = do
      (code, out) <- readProcessInterleaved config
      case code of
        ExitFailure 128
          | "index.lock" `BS.isInfixOf` BSL.toStrict out -> do
            threadDelay 100000
            go
        ExitSuccess -> return $ (BSL.toStrict >>> T.decodeUtf8) out
        ExitFailure _ ->
          Control.Exception.throw $ ExitCodeException code config out out
