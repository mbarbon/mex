module Mex.Subtitles (
  convertSubtitle,
  extractInternalSubs,
  hardsubInternalSub,
  extractAndConvertViableInternalSub
) where

import qualified Data.ByteString.Char8 as Char8

import Control.Exception (handle, IOException)
import Control.Monad (forM)
import Data.List (intersperse, sort)
import System.Posix (getFileStatus, fileSize, FileOffset)
import System.FilePath (dropExtension, takeExtension, addExtension)
import System.Posix.Files (createSymbolicLink)

import Mex.CommandTree
import Mex.Commands
import Mex.MediaInfo

convertSubtitle :: ExternalSubtitle -> CommandTree
convertSubtitle ExternalSubtitle { subFormat = MediaFormat "SRT", subFile = source } = noopCommand source
convertSubtitle es@ExternalSubtitle { subFile = file, subFormat = subFormat } =
  ffmpegConvertSubs es (MediaFormat "SRT")
convertSubtitle (NoSubtitles source) = noopCommand source

extractInternalSubs :: MediaInfo -> [MediaTrack] -> (CommandTree, [ExternalSubtitle])
extractInternalSubs mediainfo tracks =
  let (withFfmpeg, ffmpegOut) = ffmpegExtractSubs mediainfo tracks
      (withMkvExtract, mkvextractInt) = mkvextractSubs mediainfo tracks
      withMkvConvert = ffmpegConvertAllSubs mkvextractInt (MediaFormat "SRT")
      mkvPipeline =  withMkvExtract `andCommand` withMkvConvert
      tryEither = withFfmpeg `orCommand` mkvPipeline
   in (tryEither, ffmpegOut)

extractAndConvertViableInternalSub :: MediaInfo -> [MediaTrack] -> CommandTree
extractAndConvertViableInternalSub mediainfo tracks =
  let (extract, subs) = extractInternalSubs mediainfo tracks
      filtered = extract `andCommand` removeHtmlTags subs
   in filtered `andCommand` linkFirstViableSub (map subFile subs)

hardsubInternalSub :: MediaInfo -> [MediaTrack] -> CommandTree
hardsubInternalSub mediainfo tracks =
  -- picking the last is a broken heuristic
  let subtitles = filter (("Text" ==) . mediaType) tracks
   in if null subtitles
        then noCommand (mediaFile mediainfo)
        else ffmpegHardsub mediainfo (last subtitles)

linkFirstViableSub :: [FilePath] -> CommandTree
linkFirstViableSub subs = internalCommand description action
  where
    description = "ln -s one of (" ++ (concat (intersperse " " subs)) ++ ")"
    action :: IO (Bool, FailureDescription)
    action = do
      sortedBySize <- sortDescendingSize subs
      viableSrt <- findViableSrt sortedBySize
      case viableSrt of
        Nothing -> return (False, describeFailure "No viable subtitle")
        Just f  -> symlinkToTarget f

    symlinkToTarget path = do
      let extension = takeExtension path
      let base = (dropExtension . dropExtension) path
      let target = (addExtension base extension)
      handle failed
        (createSymbolicLink path target >> commandSuccess)

    failed :: IOException -> IO (Bool, FailureDescription)
    failed _ = return (False, describeFailure "symlink failed")

isViableSrt :: FilePath -> IO Bool
isViableSrt path = do
  contents <- Char8.readFile path
  return ((length (longLines contents)) < 10)
  where
    isLong = (> 500) . Char8.length
    longLines s =
      let splitLines = Char8.lines s
       in filter isLong splitLines

findViableSrt :: [FilePath] -> IO (Maybe FilePath)
findViableSrt (s:ss) = do
  viable <- isViableSrt s
  if viable
    then return (Just s)
    else findViableSrt ss
findViableSrt _ = return Nothing

sortDescendingSize :: [FilePath] -> IO [FilePath]
sortDescendingSize files = do
  withSize <- forM files fileAndSize
  return (map snd (sort withSize))
  where
    fileAndSize :: FilePath -> IO (FileOffset, FilePath)
    fileAndSize f = do
      status <- getFileStatus f
      return (-(fileSize status), f)
