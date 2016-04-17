module Mex (main) where

import Prelude hiding (lookup, concat)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.FilePath (replaceExtension, takeExtension, replaceDirectory, combine)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesFileExist, getDirectoryContents)
import Text.XML (parseText, elementName, elementAttributes, def, nameLocalName, Element, Name, Node(NodeElement))
import Text.XML.Cursor (fromDocument, checkElement, element, node, content, descendant, Cursor, (&/), ($.//))
import Data.Maybe (catMaybes)
import Data.Map.Lazy (member, lookup)
import Data.Text (pack, unpack, concat)
import Data.String (fromString)
import Data.List (elemIndex, intercalate)
import Debug.Trace
import Control.Monad (filterM, liftM, foldM)

data MediaFormat = MediaFormat String
  deriving (Eq, Show)
data ExternalSubtitle = ExternalSubtitle { subFile :: String, subFormat :: MediaFormat }
  deriving (Eq, Show)
data MediaTrack = MediaTrack { trackId, mediaType :: String, trackFormat :: MediaFormat }
  deriving (Eq, Show)
data MediaInfo = MediaInfo { mediaFile :: String, tracks :: [MediaTrack], subtitles :: [ExternalSubtitle] }
  deriving (Eq, Show)
data CommandOperation = And | Or deriving (Eq, Read, Show)
data CommandTree = CommandChain CommandOperation [CommandTree]
                 | Command String [String]
                 | Noop
                 | None
  deriving (Eq, Read, Show)

main =
  do directories <- getArgs
     mapM processDir directories
  where
    processDir :: String -> IO ()
    processDir directory =
      do workitems <- workList directory
         let commands = commandList workitems
         mapM runVerbose commands
         return ()

mediaFiles = ["mkv", "avi", "mp4"]
subtitleFiles  = ["srt", "ass", "ssa"]

extForFormat :: MediaFormat -> String
extForFormat (MediaFormat "SRT") = "srt"
extForFormat (MediaFormat "ASS") = "ass"
extForFormat (MediaFormat "SSA") = "ssa"

formatForExt :: String -> MediaFormat
formatForExt "srt" = MediaFormat "SRT"
formatForExt "ass" = MediaFormat "ASS"
formatForExt "ssa" = MediaFormat "SSA"

isTextSubtitle :: MediaFormat -> Bool
isTextSubtitle (MediaFormat format) =
  format == "SRT" || format == "ASS" || format == "SSA"

trackForId :: MediaInfo -> String -> Maybe MediaTrack
trackForId mediainfo trkid =
  case filter (\t -> trkid == trackId t) (tracks mediainfo) of
    (t:[]) -> Just t
    _      -> Nothing

trackIndex :: MediaInfo -> String -> String -> Maybe Int
trackIndex mediainfo mediatype trkid =
  elemIndex trkid (map trackId (filter (\t -> mediatype == mediaType t) (tracks mediainfo)))

workList :: FilePath -> IO [MediaInfo]
workList directory =
      do files <- listMediaFiles directory
         mapM makeWorkEntry files
  where
    makeWorkEntry :: FilePath -> IO MediaInfo
    makeWorkEntry file =
      mediaInfo file >>= addExternalSubs

listMediaFiles :: FilePath -> IO [FilePath]
listMediaFiles dir =
  let isMediaFile :: FilePath -> Bool
      isMediaFile f =
        case takeExtension f of
          '.' : ext -> ext `elem` mediaFiles
          ""        -> False
    in do files <- getDirectoryContents dir
          return [file | file <- map (combine dir) files, isMediaFile file]
  
addExternalSubs :: MediaInfo -> IO MediaInfo
addExternalSubs mi@MediaInfo { mediaFile = file } =
  do externalSubs <- findExternalSubs file
     return mi { subtitles = externalSubs }
  where
    findExternalSubs :: FilePath -> IO [ExternalSubtitle]
    findExternalSubs file =
      filterM (doesFileExist . subFile) [
        ExternalSubtitle { subFile = replaceExtension file i,
                           subFormat = formatForExt i } | i <- subtitleFiles
      ]

mediaInfo :: FilePath -> IO MediaInfo
mediaInfo file =
  let isTrack :: Element -> Bool
      isTrack e = (unpack . nameLocalName . elementName) e == "track" &&
                  member (fromString "type") (elementAttributes e)
      firstChild :: String -> Cursor -> Maybe Cursor
      firstChild name e =
        case ($.// (element (fromString name))) e of
          x : xs -> Just x
          _      -> Nothing
      textContent :: Cursor -> String
      textContent = unpack . concat . ($.// content)
      getTrack :: Cursor -> Maybe MediaTrack
      getTrack n =
        let NodeElement e = node n
         in do trackId <- firstChild "ID" n
               format <- firstChild "Format" n
               mediaType <- lookup (fromString "type") (elementAttributes e)
               return MediaTrack { trackId   = textContent trackId,
                                   trackFormat = MediaFormat (textContent format),
                                   mediaType = unpack mediaType }
   in do (exitCode, xmlOutput, _) <- readProcessWithExitCode "mediainfo" ["--Output=XML", file] ""
         let Right document = parseText def (fromString xmlOutput)
         let maybeTracks = ($.// (checkElement isTrack)) (fromDocument document)
             allTracks = catMaybes (map getTrack maybeTracks)
          in return MediaInfo { mediaFile = file, tracks = allTracks, subtitles = [] }

mkvextractSubs :: MediaInfo -> MediaTrack -> (CommandTree, ExternalSubtitle)
mkvextractSubs mediainfo track =
  let index = (read (trackId track)) - 1
      path = (mediaFile mediainfo)
      format = trackFormat track
      fileExt = extForFormat $ format
      outFile = replaceExtension path fileExt
      command = Command "mkvextract" ["tracks", path, (show index) ++ ":" ++ outFile]
      result = ExternalSubtitle { subFormat = format, subFile = outFile }
   in (command, result)

ffmpegExtractSubs :: MediaInfo -> MediaTrack -> (CommandTree, ExternalSubtitle)
ffmpegExtractSubs mediainfo track =
  let Just index = trackIndex mediainfo "Text" (trackId track)
      path = (mediaFile mediainfo)
      outFile = replaceExtension path "srt"
      command = Command "ffmpeg" ["-y", "-i", path, "-map", "0:s:" ++ (show index), "-an", "-vn", "-c:s", "srt", outFile]
      result = ExternalSubtitle { subFormat = MediaFormat "SRT", subFile = outFile }
   in (command, result)

ffmpegConvertSubs :: ExternalSubtitle -> MediaFormat -> CommandTree
ffmpegConvertSubs es@ExternalSubtitle { subFormat = subFormat } format | subFormat == format = Noop
ffmpegConvertSubs source format =
  let srcFile = (subFile source)
      outFile = replaceExtension srcFile (extForFormat format)
   in Command "ffmpeg" ["-y", "-i", srcFile, outFile]

ffmpegHardsub :: MediaInfo -> MediaTrack -> CommandTree
ffmpegHardsub mediainfo track =
  let Just index = trackIndex mediainfo "Text" (trackId track)
      path = (mediaFile mediainfo)
      tempFile = (replaceExtension path (".tmp" ++ (takeExtension path)))
      filterSpec = "[0:v][0:s:" ++ (show index) ++ "]overlay[v]"
      command = Command "ffmpeg" ["-y", "-i", path, "-filter_complex", filterSpec, "-map", "[v]", "-map", "0:a", "-c:a", "copy", tempFile]
   in concatCommand And command (renameFile tempFile path)

removeHtmlTags :: ExternalSubtitle -> CommandTree
removeHtmlTags es | isTextSubtitle (subFormat es) =
  Command "sed" ["-i.orig", "s/<[^>]\\+>//g", (subFile es)]
removeHtmlTags _ = Noop

renameFile :: String -> String -> CommandTree
renameFile from to = Command "mv" [from, to]

commandList :: [MediaInfo] -> [CommandTree]
commandList = map makeCommand
  where
    makeCommand :: MediaInfo -> CommandTree
    makeCommand mediainfo =
      let extractSubtitlesCmd = extractSubtitles mediainfo
       in extractSubtitlesCmd
    extractSubtitles mediainfo@MediaInfo {subtitles = subtitles } =
      let convertExternalCmd = bestCommand (map convertSubtitle subtitles)
          convertInternalCmd =
            -- picking the last is a broken heuristic
            let internalAlternatives = extractInternal mediainfo (tracks mediainfo)
             in if null internalAlternatives
                  then None
                  else let (extractInternalCmd, extractedSub) = last internalAlternatives
                        in concatCommand And extractInternalCmd (convertSubtitle extractedSub)
          hardsubInternalCmd = hardsubInternal mediainfo (tracks mediainfo)
       in case (convertExternalCmd, convertInternalCmd, hardsubInternalCmd) of
            (None, None, None)    -> None
            (None, None, hardsub) -> hardsub
            (None, extract, _)    -> extract
            (convert, _, _)       -> convert
    convertSubtitle :: ExternalSubtitle -> CommandTree
    convertSubtitle ExternalSubtitle { subFormat = MediaFormat "SRT" } = Noop
    convertSubtitle es@ExternalSubtitle { subFile = file, subFormat = subFormat } =
      ffmpegConvertSubs es (MediaFormat "SRT")
    extractInternal :: MediaInfo -> [MediaTrack] -> [(CommandTree, ExternalSubtitle)]
    extractInternal mediainfo (trk@MediaTrack { mediaType = "Text", trackFormat = format }:ts) | not (isTextSubtitle format) = extractInternal mediainfo ts
    extractInternal mediainfo (trk@MediaTrack { mediaType = "Text" }:ts) =
      let (withFfmpeg, ffmpegOut) = ffmpegExtractSubs mediainfo trk
          (withMkvExtract, mkvextractInt) = mkvextractSubs mediainfo trk
          withMkvConvert = ffmpegConvertSubs mkvextractInt (MediaFormat "SRT")
          mkvPipeline = concatCommand And withMkvExtract withMkvConvert
          tryEither = concatCommand Or withFfmpeg mkvPipeline
          deHtml = concatCommand And tryEither (removeHtmlTags ffmpegOut)
       in (deHtml, ffmpegOut) : extractInternal mediainfo ts
    extractInternal mediainfo (t:ts) = extractInternal mediainfo ts
    extractInternal _ [] = []
    hardsubInternal :: MediaInfo -> [MediaTrack] -> CommandTree
    hardsubInternal mediainfo tracks =
      -- picking the last is a broken heuristic
      let subtitles = filter (("Text" ==) . mediaType) tracks
       in if null subtitles
            then None
            else ffmpegHardsub mediainfo (last subtitles)

concatCommands :: [CommandTree] -> CommandTree
concatCommands = foldr (concatCommand And) Noop

bestCommand :: [CommandTree] -> CommandTree
bestCommand = foldr pickBest None
  where
    pickBest None cmd2 = cmd2
    pickBest cmd1 None = cmd1
    pickBest Noop cmd2 = Noop
    pickBest cmd1 Noop = Noop
    -- arbitrary
    pickBest cmd1 cmd2 = cmd1

concatCommand :: CommandOperation -> CommandTree -> CommandTree -> CommandTree
concatCommand op None cmd2 = None
concatCommand op cmd1 None = None
concatCommand op Noop cmd2 = cmd2
concatCommand op cmd1 Noop = cmd1
concatCommand op cmd1 cmd2 = CommandChain op (cmd1 : cmd2 : [])

shellScript :: [CommandTree] -> String
shellScript commands = intercalate "\n" (map shellCommand commands)

shellCommand :: CommandTree -> String
shellCommand None = "# can't do anything"
shellCommand Noop = "# nothing to do"
shellCommand (Command command args) =
  command ++ " '" ++ (intercalate "' '" args) ++ "'"
shellCommand (CommandChain And commands) =
  intercalate " && \\\n    " (map shellCommand commands)
shellCommand (CommandChain Or commands) =
  intercalate " || \\\n    " (map shellCommand commands)

runCommand :: CommandTree -> IO (Bool, String)
runCommand None = return (False, "'None' commands can't be executed")
runCommand Noop = return (True, "")
runCommand (Command command args) =
  do (exitCode, output, error) <- readProcessWithExitCode command args ""
     case exitCode of
       ExitSuccess   -> return (True, output)
       ExitFailure _ -> return (False, if null error then output else error)
runCommand (CommandChain And commands) =
  foldM mergeResult (True, "") commands
  where
    mergeResult fail@(False, _) _ = return fail
    mergeResult _ command = runCommand command
runCommand (CommandChain Or commands) =
  foldM mergeResult (False, "") commands
  where
    mergeResult success@(True, _) _ = return success
    mergeResult _ command = runCommand command

runVerbose :: CommandTree -> IO (Bool, String)
runVerbose command =
  do putStrLn (shellCommand command)
     (success, output) <- runCommand command
     if success
       then return ()
       else putStrLn ("Command failed:\n\n" ++ output)
     return (success, output)
