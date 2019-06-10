{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Nix
  ( nixEvalET
  , assertNewerVersion
  , lookupAttrPath
  , getDerivationFile
  , getMaintainers
  , getOldHash
  , getSrcUrl
  , getSrcUrls
  , getIsBroken
  , getOutpaths
  , parseStringList
  , build
  , getDescription
  , getHomepage
  , cachix
  , assertOneOrFewerFetcher
  , getHashFromBuild
  , assertOldVersionOn
  , resultLink
  , sha256Zero
  ) where

import OurPrelude
import Prelude hiding (log)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import System.Exit
import Text.Parsec (parse)
import Text.Parser.Combinators
import Text.Parser.Token
import Utils (UpdateEnv(..), eTError, overwriteErrorT, srcOrMain)

data Raw
  = Raw
  | NoRaw

rawOpt :: Raw -> [String]
rawOpt Raw = ["--raw"]
rawOpt NoRaw = []

nixEvalET :: Members '[ Lift IO, Error Text] r => Raw -> Text -> Sem r Text
nixEvalET raw expr =
  ourReadProcessInterleaved_
    (proc "nix" (["eval", "-f", "."] <> rawOpt raw <> [T.unpack expr])) <&>
  T.strip &
  overwriteErrorT ("nix eval failed for " <> expr <> " ")

-- Error if the "new version" is actually newer according to nix
assertNewerVersion :: Members '[ Lift IO, Error Text] r => UpdateEnv -> Sem r ()
assertNewerVersion updateEnv = do
  versionComparison <-
    nixEvalET
      NoRaw
      ("(builtins.compareVersions \"" <> newVersion updateEnv <> "\" \"" <>
       oldVersion updateEnv <>
       "\")")
  case versionComparison of
    "1" -> return ()
    a ->
      throw
        (newVersion updateEnv <> " is not newer than " <> oldVersion updateEnv <>
         " according to Nix; versionComparison: " <>
         a <>
         " ")

-- This is extremely slow but gives us the best results we know of
lookupAttrPath :: Members '[ Lift IO, Error Text] r => UpdateEnv -> Sem r Text
lookupAttrPath updateEnv =
  proc
    "nix-env"
    [ "-qa"
    , (packageName updateEnv <> "-" <> oldVersion updateEnv) & T.unpack
    , "-f"
    , "."
    , "--attr-path"
    , "--arg"
    , "config"
    , "{ allowBroken = true; allowUnfree = true; allowAliases = false; }"
    ] &
  ourReadProcessInterleaved_ <&> (T.lines >>> head >>> T.words >>> head) &
  overwriteErrorT "nix-env -q failed to find package name with old version "

getDerivationFile :: Members '[ Lift IO, Error Text] r => Text -> Sem r FilePath
getDerivationFile attrPath =
  proc "env" ["EDITOR=echo", "nix", "edit", attrPath & T.unpack, "-f", "."] &
  ourReadProcessInterleaved_ <&> (T.strip >>> T.unpack) &
  overwriteErrorT "Couldn't find derivation file. "

getHash :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getHash =
  srcOrMain
    (\attrPath -> nixEvalET Raw ("pkgs." <> attrPath <> ".drvAttrs.outputHash"))

getOldHash :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getOldHash attrPath =
  getHash attrPath &
  overwriteErrorT
    ("Could not find old output hash at " <> attrPath <>
     ".src.drvAttrs.outputHash or .drvAttrs.outputHash.")

getMaintainers :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getMaintainers attrPath =
  nixEvalET
    Raw
    ("(let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh pkgs." <>
     attrPath <>
     ".meta.maintainers or []))))") &
  overwriteErrorT ("Could not fetch maintainers for" <> attrPath)

parseStringList ::
     Members '[ Lift IO, Error Text] r => Text -> Sem r (Vector Text)
parseStringList list = do
  parse nixStringList ("nix list " ++ T.unpack list) list & \case
    Left error -> throw (tshow error)
    Right vec -> return vec

nixStringList :: TokenParsing m => m (Vector Text)
nixStringList = V.fromList <$> brackets (many stringLiteral)

getOutpaths :: Members '[ Lift IO, Error Text] r => Text -> Sem r (Vector Text)
getOutpaths attrPath = do
  list <- nixEvalET NoRaw (attrPath <> ".outputs")
  outputs <- parseStringList list
  V.sequence $ fmap (\o -> nixEvalET Raw (attrPath <> "." <> o)) outputs

readNixBool :: Member (Error Text) r => Text -> Sem r Bool
readNixBool "true" = return True
readNixBool "false" = return False
readNixBool a = throw ("Failed to read expected nix boolean " <> a <> " ")

getIsBroken :: Members '[ Lift IO, Error Text] r => Text -> Sem r Bool
getIsBroken attrPath =
  (do result <-
        nixEvalET
          NoRaw
          ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
           ".meta.broken or false)")
      readNixBool result) &
  overwriteErrorT ("Could not get meta.broken for attrpath " <> attrPath)

getDescription :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getDescription attrPath =
  nixEvalET
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.description or \"\")") &
  overwriteErrorT ("Could not get meta.description for attrpath " <> attrPath)

getHomepage :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getHomepage attrPath =
  nixEvalET
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.homepage or \"\")") &
  overwriteErrorT ("Could not get meta.homepage for attrpath " <> attrPath)

getSrcUrl :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getSrcUrl =
  srcOrMain
    (\attrPath ->
       nixEvalET
         Raw
         ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <>
          ".drvAttrs.urls 0)"))

getSrcAttr :: Members '[ Lift IO, Error Text] r => Text -> Text -> Sem r Text
getSrcAttr attr =
  srcOrMain (\attrPath -> nixEvalET NoRaw ("pkgs." <> attrPath <> "." <> attr))

getSrcUrls :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getSrcUrls = getSrcAttr "urls"

buildCmd :: Text -> ProcessConfig () () ()
buildCmd attrPath =
  silently $
  proc
    "nix-build"
    [ "--option"
    , "sandbox"
    , "true"
    , "--option"
    , "restrict-eval"
    , "true"
    , "--arg"
    , "config"
    , "{ allowBroken = true; allowUnfree = true; allowAliases = false; }"
    , "-A"
    , attrPath & T.unpack
    ]

log :: Text -> ProcessConfig () () ()
log attrPath = proc "nix" ["log", "-f", ".", attrPath & T.unpack]

build :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
build attrPath =
  (buildCmd attrPath & runProcess_ & tryIOTextET) `catch`
  const
    (do _ <- buildFailedLog
        throw "nix log failed trying to get build logs ")
  where
    buildFailedLog = do
      buildLog <-
        ourReadProcessInterleaved_ (log attrPath) &
        fmap (T.lines >>> reverse >>> take 30 >>> reverse >>> T.unlines)
      throw ("nix build failed.\n" <> buildLog <> " ")

cachix :: Members '[ Lift IO, Error Text] r => Text -> Sem r ()
cachix resultPath =
  (setStdin
     (byteStringInput (TL.encodeUtf8 (TL.fromStrict resultPath)))
     (shell "cachix push r-ryantm") &
   runProcess_ &
   tryIOTextET) &
  overwriteErrorT "pushing to cachix failed"

numberOfFetchers :: Text -> Int
numberOfFetchers derivationContents =
  countUp "fetchurl {" + countUp "fetchgit {" + countUp "fetchFromGitHub {"
  where
    countUp x = T.count x derivationContents

assertOneOrFewerFetcher :: MonadIO m => Text -> FilePath -> ExceptT Text m ()
assertOneOrFewerFetcher derivationContents derivationFile =
  tryAssert
    ("More than one fetcher in " <> T.pack derivationFile)
    (numberOfFetchers derivationContents <= 1)

assertOldVersionOn ::
     MonadIO m => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertOldVersionOn updateEnv branchName contents =
  tryAssert
    ("Old version not present in " <> branchName <> " derivation file.")
    (oldVersionPattern `T.isInfixOf` contents)
  where
    oldVersionPattern = oldVersion updateEnv <> "\""

resultLink :: Members '[ Lift IO, Error Text] r => Sem r Text
resultLink =
  T.strip <$>
  (ourReadProcessInterleaved_ "readlink ./result" `catch`
   const (ourReadProcessInterleaved_ "readlink ./result-bin")) `catch`
  const (throw "Could not find result link. ")

sha256Zero :: Text
sha256Zero = "0000000000000000000000000000000000000000000000000000"

-- fixed-output derivation produced path '/nix/store/fg2hz90z5bc773gpsx4gfxn3l6fl66nw-source' with sha256 hash '0q1lsgc1621czrg49nmabq6am9sgxa9syxrwzlksqqr4dyzw4nmf' instead of the expected hash '0bp22mzkjy48gncj5vm9b7whzrggcbs5pd4cnb6k8jpl9j02dhdv'
getHashFromBuild :: Members '[ Lift IO, Error Text] r => Text -> Sem r Text
getHashFromBuild =
  srcOrMain
    (\attrPath -> do
       (exitCode, _, stderr) <- buildCmd attrPath & readProcess
       when (exitCode == ExitSuccess) $ throw "build succeeded unexpectedly"
       let stdErrText = bytestringToText stderr
       let firstSplit = T.splitOn "got:    sha256:" stdErrText
       firstSplitSecondPart <-
         eTError $
         tryAt
           ("stderr did not split as expected full stderr was: \n" <> stdErrText)
           firstSplit
           1
       let secondSplit = T.splitOn "\n" firstSplitSecondPart
       eTError $
         tryHead
           ("stderr did not split second part as expected full stderr was: \n" <>
            stdErrText <>
            "\nfirstSplitSecondPart:\n" <>
            firstSplitSecondPart)
           secondSplit)
