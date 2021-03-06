module Mex (main) where

import Control.Monad (liftM, forM_, mapM_, filterM)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, removeFile, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.FilePath (replaceDirectory, takeDirectory, (</>))
import System.Posix.Files (createSymbolicLink)

import Mex.CachedMediaInfo
import Mex.Commands
import Mex.CommandTree
import Mex.MediaInfo
import Mex.Subtitles
import Mex.Transcode
import Mex.Scan

data Options = Options {
  subPreferMkvextract :: Bool,
  forceHardsub :: Bool,
  forceVideo :: Bool
} deriving (Show)
defaultOptions = Options {
  subPreferMkvextract = False,
  forceHardsub = False,
  forceVideo = False
}

commands :: [(String, [String] -> IO ())]
commands = [
    ("process", processDirectory),
    ("scan",    scanTree),
    ("help",    showHelp)
  ]

main =
  do args <- getArgs
     case args of
       (command : commandArgs) -> runCommand command commandArgs
       _                       -> showHelp []
  where
    runCommand command args =
      case lookup command commands of
        Just action -> action args
        Nothing     -> showHelp []

processDirectory :: [String] -> IO ()
processDirectory args =
  let (options, directories) = processOptions args
   in forM_ directories $ \path ->
        traverseWith (processMediaFiles options path) path

scanTree :: [String] -> IO ()
scanTree (source : target : []) =
  do listDirectoryWith (removeDanglingEntries source target) target
     traverseWith (symlinkMediaAndSubtitleFiles source target) source
     traverseWith (ifNotEmpty processSingleDirectory) target
  where
    ifNotEmpty action [] = return ()
    ifNotEmpty action fs = action fs

    processSingleDirectory files =
      do options <- readDirectoryOptions (target </> (takeDirectory (head files)))
         processMediaFiles options target files
scanTree _ = showHelp []

showHelp :: [String] -> IO ()
showHelp _ =
  let help = "\
\Usage: mex <command> [args]\n\
\\n\
\Commands:\n\
\    help\n\
\    process <directory> [<directory> ...]\n\
\    scan    <source directory> <target directory>\n"
   in putStr  help

removeDanglingEntries :: FilePath -> FilePath -> [FilePath] -> IO ()
removeDanglingEntries source target files = do
  filtered <- filterM missingSource files
  forM_ filtered $ \path -> do
    let fullPath = target </> path
    isDirectory <- doesDirectoryExist fullPath
    isFile <- doesFileExist fullPath
    case (isFile, isDirectory) of
      (_, True) -> removeDirectoryRecursive fullPath
      (True, _) -> removeFile fullPath
      _         -> return ()
  where
    missingSource :: FilePath -> IO Bool
    missingSource path = do
      let fullSourcePath = source </> path
      fileExists <- doesFileExist fullSourcePath
      directoryExists <- doesDirectoryExist fullSourcePath
      return $ not (fileExists || directoryExists)

symlinkMediaAndSubtitleFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
symlinkMediaAndSubtitleFiles source target files =
  forM_ (filter isMediaOrSubtitleFile files) $ \file -> do
    let targetPath = target </> file
    fileExists <- doesFileExist targetPath
    if not fileExists
      then do
        createDirectoryIfMissing True (takeDirectory targetPath)
        createSymbolicLink (source </> file) targetPath
      else return ()

processMediaFiles :: Options -> FilePath -> [FilePath] -> IO ()
processMediaFiles options source files =
  do workItems <- workList $ map (source </>) (filter isMediaFile files)
     mapM_ runVerbose (commandList options workItems)

processOptions :: [String] -> (Options, [String])
processOptions args = consumeArgs defaultOptions args
  where
    consumeArgs options ("--force-video":args) =
      consumeArgs (options { forceVideo = True }) args
    consumeArgs options ("--force-hardsub":args) =
      consumeArgs (options { forceHardsub = True }) args
    consumeArgs options ("--sub-prefer-mkvextract":args) =
      consumeArgs (options { subPreferMkvextract = True }) args
    consumeArgs options args = (options, args)

readDirectoryOptions :: String -> IO Options
readDirectoryOptions directory = do
  let fullPath = directory </> "mex.options"
  exists <- doesFileExist fullPath
  contents <- if exists
                then readFile fullPath
                else return ""
  let (options, rest) = processOptions (filter interestingLine (lines contents))
  if rest == []
    then return options
    else fail ("Unknown options " ++ (show rest))
  where
    interestingLine "" = False
    interestingLine (' ':_) = False
    interestingLine ('#':_) = False
    interestingLine _ = True

-- XXX configuration
isGoodInternalSubtitle :: MediaFormat -> Bool
isGoodInternalSubtitle (MediaFormat "SRT") = True
isGoodInternalSubtitle (MediaFormat "UTF-8") = True
isGoodInternalSubtitle _ = False

isGoodExternalSubtitle :: MediaFormat -> Bool
isGoodExternalSubtitle (MediaFormat "SRT") = True
isGoodExternalSubtitle _ = False

isGoodAudioFormat :: MediaFormat -> Bool
isGoodAudioFormat (MediaFormat "FLAC") = False
isGoodAudioFormat _ = True

workList :: [FilePath] -> IO [MediaInfo]
workList files =
  mapM makeWorkEntry (filter isMediaFile files)
  where
    makeWorkEntry :: FilePath -> IO MediaInfo
    makeWorkEntry file =
      cachedMediaInfo file >>= addExternalSubs

commandList :: Options -> [MediaInfo] -> [CommandTree]
commandList options = map makeCommand
  where
    makeCommand :: MediaInfo -> CommandTree
    makeCommand mediainfo =
      let goodExternal = filter (isGoodExternalSubtitle . subFormat) (subtitles mediainfo)
          goodInternal = filter (isGoodInternalSubtitle . trackFormat) (tracks mediainfo)
          transcode = if (symbolicLink mediainfo)
                        then maybeTranscodeAudio options mediainfo (maybeTranscodeVideo options mediainfo (noopTranscode mediainfo))
                        else noopTranscode mediainfo
       in if (null goodExternal) && (null goodInternal)
            then extractAndConvertSubtitles options mediainfo transcode
            else transcodeCommand transcode

extractAndConvertSubtitles options mediainfo@MediaInfo { subtitles = subtitles } transcode =
  let convertExternalCmd = viableCommand (map convertSubtitle subtitles)
      internalSubtitles = filteredSubtitles mediainfo
      convertInternalCmd =
        let textSubs = filter (isTextSubtitle . trackFormat) internalSubtitles
         in if null textSubs
              then noCommand (mediaFile mediainfo)
              else extractAndConvertViableInternalSub (subPreferMkvextract options) mediainfo textSubs
      hardsubInternalCmd = transcodeCommand (hardsubInternalSub transcode internalSubtitles)
      forcedHardsub = (hasInternalSubs internalSubtitles) && (forceHardsub options)
      hasSubConversion = (isViableCommand convertExternalCmd) || (isViableCommand convertInternalCmd)
      chosen = viableCommand [convertExternalCmd, convertInternalCmd]
   in if hasSubConversion && not forcedHardsub
        then chosen `andCommand` (transcodeCommand transcode)
        else hardsubInternalCmd

maybeTranscodeVideo :: Options -> MediaInfo -> Transcode -> Transcode
maybeTranscodeVideo options mediainfo transcode =
  if (forceVideo options) || needsTranscode
    then transcodeVideo transcode basicX264
    else transcode
  where
    needsTranscode = (any levelOrRefFramesTooHigh (tracks mediainfo)) ||
                     (any unsupportedVideoFormat (tracks mediainfo))

    -- XXX configuration
    levelOrRefFramesTooHigh (MediaTrack { mediaType = "Video", referenceFrames = Just frames, profileLevel = Just ("High 10", level) }) =
      level >= 4 || frames > 8
    levelOrRefFramesTooHigh _ = False

    -- XXX configuration
    unsupportedVideoFormat (MediaTrack { mediaType = "Video", trackFormat = MediaFormat "HEVC" }) = True
    unsupportedVideoFormat _ = False

maybeTranscodeAudio :: Options -> MediaInfo -> Transcode -> Transcode
maybeTranscodeAudio options mediainfo transcode =
  if needsTranscode
    then transcodeAudio transcode basicAAC
    else transcode
  where
    needsTranscode = not (any (isGoodAudioFormat . trackFormat) (audioTracks mediainfo))

filteredSubtitles :: MediaInfo -> [MediaTrack]
filteredSubtitles mediaInfo =
  filter hasRightLanguage (filter isSubtitleTrack (tracks mediaInfo))

  where
    hasRightLanguage :: MediaTrack -> Bool
    hasRightLanguage track =
      case (language track) of
        Nothing   -> True
        Just lang -> elem lang preferredSubtitles

-- XXX configuration
basicX264 = ["libx264", "-preset", "slow", "-level", "4.1", "-crf", "23"]

basicAAC = ["aac", "-b:a", "192k", "-ac", "2"]

preferredSubtitles = ["English", "Italian"]
