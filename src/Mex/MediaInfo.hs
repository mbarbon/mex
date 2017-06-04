module Mex.MediaInfo (
  MediaFormat(..),
  MediaTrack(..),
  MediaInfo(..),
  ExternalSubtitle(..),
  mediaInfo,
  addExternalSubs,
  extForFormat,
  formatForExt,
  isSubtitleTrack,
  isTextSubtitle,
  isMediaFile,
  isMediaOrSubtitleFile,
  trackIndex,
  audioTracks,
) where

import Prelude hiding (lookup, concat)

import qualified Data.ByteString as ByteString

import Control.Monad (filterM)
import Data.ByteString.Lazy (fromStrict)
import Data.List (elemIndex)
import Data.Map.Lazy (member, lookup)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (pack, unpack, concat)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (replaceExtension, takeExtension)
import System.Posix.Files (isSymbolicLink, getSymbolicLinkStatus)
import System.Process.ByteString (readProcessWithExitCode)
import Text.XML (parseLBS, elementName, elementAttributes, def, nameLocalName, Element, Name, Node(NodeElement))
import Text.XML.Cursor (fromDocument, checkElement, element, node, content, descendant, Cursor, (&/), ($.//))

data MediaFormat = MediaFormat String
  deriving (Eq, Read, Show)
data ExternalSubtitle = ExternalSubtitle { subFile :: String, subFormat :: MediaFormat }
                      | NoSubtitles String
  deriving (Eq, Read, Show)
data MediaTrack = MediaTrack { trackId, mediaType :: String, trackFormat :: MediaFormat,  referenceFrames :: Maybe Int, profileLevel :: Maybe (String, Float) }
  deriving (Eq, Read, Show)
data MediaInfo = MediaInfo {
  mediaFile :: String,
  tracks :: [MediaTrack],
  subtitles :: [ExternalSubtitle],
  symbolicLink :: Bool
} deriving (Eq, Read, Show)

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

isSubtitleTrack :: MediaTrack -> Bool
isSubtitleTrack = ("Text" ==) . mediaType

isTextSubtitle :: MediaFormat -> Bool
isTextSubtitle (MediaFormat format) =
  format == "SRT" || format == "ASS" || format == "SSA" || format == "UTF-8"

isMediaOrSubtitleFile :: FilePath -> Bool
isMediaOrSubtitleFile f =
  case takeExtension f of
    '.' : ext -> ext `elem` (mediaFiles ++ subtitleFiles)
    ""        -> False

isMediaFile :: FilePath -> Bool
isMediaFile f =
  case takeExtension f of
    '.' : ext -> ext `elem` mediaFiles
    ""        -> False

audioTracks :: MediaInfo -> [MediaTrack]
audioTracks = (filter ((== "Audio") . mediaType)) . tracks

trackIndex :: MediaInfo -> String -> String -> Maybe Int
trackIndex mediainfo mediatype trkid =
  elemIndex trkid (map trackId (filter (\t -> mediatype == mediaType t) (tracks mediainfo)))

mediaInfo :: FilePath -> IO MediaInfo
mediaInfo file = do
  (exitCode, xmlOutput, _) <- readProcessWithExitCode "mediainfo" ["--Output=XML", file] ByteString.empty
  status <- getSymbolicLinkStatus file
  case (exitCode, parseLBS def (fromStrict xmlOutput)) of
    (ExitSuccess, Right document) ->
      let maybeTracks = ($.// (checkElement isTrack)) (fromDocument document)
          allTracks = catMaybes (map getTrack maybeTracks)
       in return MediaInfo { mediaFile = file, tracks = allTracks, subtitles = [], symbolicLink = isSymbolicLink status }
    (_, _) -> return MediaInfo { mediaFile = file, tracks = [], subtitles = [], symbolicLink = isSymbolicLink status }
  where
    isTrack :: Element -> Bool
    isTrack e = (unpack . nameLocalName . elementName) e == "track" &&
                member (fromString "type") (elementAttributes e)

    firstChild :: String -> Cursor -> Maybe Cursor
    firstChild name e =
      case ($.// (element (fromString name))) e of
        x : xs -> Just x
        _      -> Nothing

    textContent :: Cursor -> String
    textContent = unpack . concat . ($.// content)

    getRefFrames :: Cursor -> Maybe Int
    getRefFrames n = do
      node <- firstChild "Format_settings__ReFrames" n
      case words (textContent node) of
        (count:["frames"]) -> return ((read count) :: Int)
        _                  -> Nothing

    splitOnAt :: String -> String -> (String, String)
    splitOnAt acc ('@':ss) = (reverse acc, ss)
    splitOnAt acc (c:ss) = splitOnAt (c:acc) ss
    splitOnAt acc ""     = (reverse acc, "")

    getProfileLevel :: Cursor -> Maybe (String, Float)
    getProfileLevel n = do
      node <- firstChild "Format_profile" n
      case splitOnAt "" (textContent node) of
        (profile, 'L':levelString) -> do
          level <- maybeFloat levelString
          return (profile, level)
        _                    -> Nothing

    maybeFloat :: String -> Maybe Float
    maybeFloat s =
      case (reads s) :: [(Float, String)] of
        (f, ""):_ -> Just f
        _         -> Nothing

    getTrack :: Cursor -> Maybe MediaTrack
    getTrack n =
      let NodeElement e = node n
       in do trackId <- firstChild "ID" n
             format <- firstChild "Format" n
             mediaType <- lookup (fromString "type") (elementAttributes e)
             return MediaTrack { trackId   = textContent trackId,
                                 trackFormat = MediaFormat (textContent format),
                                 mediaType = unpack mediaType,
                                 referenceFrames = getRefFrames n,
                                 profileLevel = getProfileLevel n }

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
