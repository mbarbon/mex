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
       in if (null goodExternal) && (null goodInternal)
            then extractAndConvertSubtitles mediainfo
            else noopCommand (mediaFile mediainfo)

extractAndConvertSubtitles mediainfo@MediaInfo {subtitles = subtitles } =
  let convertExternalCmd = viableCommand (map convertSubtitle subtitles)
      convertInternalCmd =
        let textSubs = (filter (isTextSubtitle . trackFormat) (tracks mediainfo))
         in if null textSubs
              then noCommand (mediaFile mediainfo)
              else extractAndConvertViableInternalSub mediainfo textSubs
      hardsubInternalCmd = hardsubInternalSub mediainfo (tracks mediainfo)
   in viableCommand [convertExternalCmd, convertInternalCmd, hardsubInternalCmd]
