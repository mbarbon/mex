module Mex (main) where

import Control.Monad (liftM, forM_, mapM_)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (replaceDirectory, takeDirectory, (</>))
import System.Posix.Files (createSymbolicLink)

import Mex.Commands
import Mex.CommandTree
import Mex.MediaInfo
import Mex.Subtitles
import Mex.Transcode
import Mex.Scan (traverseWith)

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
processDirectory directories =
  forM_ directories $ \path ->
    traverseWith (processMediaFiles path) path

scanTree :: [String] -> IO ()
scanTree (source : target : []) =
  do traverseWith (symlinkMediaAndSubtitleFiles source target) source
     traverseWith (processMediaFiles target) target
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

processMediaFiles :: FilePath -> [FilePath] -> IO ()
processMediaFiles source files =
  do workItems <- workList $ map (source </>) (filter isMediaFile files)
     mapM_ runVerbose (commandList workItems)

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
      mediaInfo file >>= addExternalSubs

commandList :: [MediaInfo] -> [CommandTree]
commandList = map makeCommand
  where
    makeCommand :: MediaInfo -> CommandTree
    makeCommand mediainfo =
      let goodExternal = filter (isGoodExternalSubtitle . subFormat) (subtitles mediainfo)
          goodInternal = filter (isGoodInternalSubtitle . trackFormat) (tracks mediainfo)
          transcode = maybeTranscodeAudio mediainfo (maybeTranscodeVideo mediainfo (noopTranscode mediainfo))
       in if (null goodExternal) && (null goodInternal)
            then extractAndConvertSubtitles mediainfo transcode
            else transcodeCommand transcode

extractAndConvertSubtitles mediainfo@MediaInfo {subtitles = subtitles } transcode =
  let convertExternalCmd = viableCommand (map convertSubtitle subtitles)
      convertInternalCmd =
        let textSubs = (filter (isTextSubtitle . trackFormat) (tracks mediainfo))
         in if null textSubs
              then noCommand (mediaFile mediainfo)
              else extractAndConvertViableInternalSub mediainfo textSubs
      hardsubInternalCmd = transcodeCommand (hardsubInternalSub transcode (tracks mediainfo))
      chosen = viableCommand [convertExternalCmd, convertInternalCmd, hardsubInternalCmd]
      extractAndTranscode = chosen `andCommand` (transcodeCommand transcode)
   in case (isViableCommand convertExternalCmd, isViableCommand convertInternalCmd) of
       (False, False) -> chosen
       _              -> extractAndTranscode

maybeTranscodeVideo :: MediaInfo -> Transcode -> Transcode
maybeTranscodeVideo mediainfo transcode =
  if needsTranscode
    then transcodeVideo transcode basicX264
    else transcode
  where
    needsTranscode = any levelOrRefFramesTooHigh (tracks mediainfo)

    -- XXX configuration
    levelOrRefFramesTooHigh (MediaTrack { mediaType = "Video", referenceFrames = Just frames, profileLevel = Just ("High 10", level) }) =
      level > 5 && frames > 8
    levelOrRefFramesTooHigh _ = False

maybeTranscodeAudio :: MediaInfo -> Transcode -> Transcode
maybeTranscodeAudio mediainfo transcode =
  if needsTranscode
    then transcodeAudio transcode basicAAC
    else transcode
  where
    needsTranscode = not (any (isGoodAudioFormat . trackFormat) (audioTracks mediainfo))

-- XXX configuration
basicX264 = ["libx264", "-preset", "slow", "-level", "4.1", "-crf", "23"]

basicAAC = ["aac", "-b:a", "192k"]
