module Mex (main) where

import Control.Monad (liftM, forM_, mapM_)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (replaceDirectory, takeDirectory, (</>))
import System.Posix.Files (createSymbolicLink)

import Mex.Commands
import Mex.CommandTree (CommandTree, andCommand, orCommand, noopCommand, noCommand, runVerbose, viableCommand)
import Mex.MediaInfo
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
      let extractSubtitlesCmd = extractSubtitles mediainfo
       in extractSubtitlesCmd
    extractSubtitles mediainfo@MediaInfo {subtitles = subtitles } =
      let convertExternalCmd = viableCommand (map convertSubtitle subtitles)
          convertInternalCmd =
            -- picking the last is a broken heuristic
            let internalAlternatives = extractInternal mediainfo (tracks mediainfo)
             in if null internalAlternatives
                  then noCommand (mediaFile mediainfo)
                  else let (extractInternalCmd, extractedSub) = last internalAlternatives
                        in extractInternalCmd `andCommand` (convertSubtitle extractedSub)
          hardsubInternalCmd = hardsubInternal mediainfo (tracks mediainfo)
       in viableCommand [convertExternalCmd, convertInternalCmd, hardsubInternalCmd]
    convertSubtitle :: ExternalSubtitle -> CommandTree
    convertSubtitle ExternalSubtitle { subFormat = MediaFormat "SRT", subFile = source } = noopCommand source
    convertSubtitle es@ExternalSubtitle { subFile = file, subFormat = subFormat } =
      ffmpegConvertSubs es (MediaFormat "SRT")
    convertSubtitle (NoSubtitles source) = noopCommand source
    extractInternal :: MediaInfo -> [MediaTrack] -> [(CommandTree, ExternalSubtitle)]
    extractInternal mediainfo (trk@MediaTrack { mediaType = "Text", trackFormat = format }:ts) | isGoodInternalSubtitle format = (noopCommand (mediaFile mediainfo), NoSubtitles (mediaFile mediainfo)):extractInternal mediainfo ts
    extractInternal mediainfo (trk@MediaTrack { mediaType = "Text", trackFormat = format }:ts) | not (isTextSubtitle format) = extractInternal mediainfo ts
    extractInternal mediainfo (trk@MediaTrack { mediaType = "Text" }:ts) =
      let (withFfmpeg, ffmpegOut) = ffmpegExtractSubs mediainfo trk
          (withMkvExtract, mkvextractInt) = mkvextractSubs mediainfo trk
          withMkvConvert = ffmpegConvertSubs mkvextractInt (MediaFormat "SRT")
          mkvPipeline =  withMkvExtract `andCommand` withMkvConvert
          tryEither = withFfmpeg `orCommand` mkvPipeline
          deHtml = tryEither `andCommand` (removeHtmlTags ffmpegOut)
       in (deHtml, ffmpegOut) : extractInternal mediainfo ts
    extractInternal mediainfo (t:ts) = extractInternal mediainfo ts
    extractInternal _ [] = []
    hardsubInternal :: MediaInfo -> [MediaTrack] -> CommandTree
    hardsubInternal mediainfo tracks =
      -- picking the last is a broken heuristic
      let subtitles = filter (("Text" ==) . mediaType) tracks
       in if null subtitles
            then noCommand (mediaFile mediainfo)
            else ffmpegHardsub mediainfo (last subtitles)
