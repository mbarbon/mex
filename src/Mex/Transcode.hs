module Mex.Transcode (
  Transcode,
  noopTranscode,
  isNoopTranscode,
  hardsubMediaTrack,
  transcodeAudio,
  transcodeVideo,
  transcodeCommand,
) where

import System.FilePath (replaceExtension, takeExtension)

import Mex.CommandTree
import Mex.MediaInfo (MediaInfo, MediaTrack, trackIndex, mediaFile, trackId, isTextSubtitle, trackFormat)

data Codec =
    Copy
  | Recode [String]
  deriving (Show)

data Transcode = Transcode {
  mediaInfo :: MediaInfo,
  audioCodec, videoCodec, subtitlesCodec :: Codec,
  hardsubTrack :: Maybe MediaTrack,
  hardsubTrackIndex :: Maybe Int
} deriving (Show)

noopTranscode :: MediaInfo -> Transcode
noopTranscode mediainfo =
  Transcode {
    mediaInfo = mediainfo,
    audioCodec = Copy,
    videoCodec = Copy,
    subtitlesCodec = Copy,
    hardsubTrack = Nothing,
    hardsubTrackIndex = Nothing
  }

hardsubMediaTrack :: Transcode -> MediaTrack -> Transcode
hardsubMediaTrack transcode track =
  let Just index = trackIndex (mediaInfo transcode) "Text" (trackId track)
   in transcode { hardsubTrack = Just track, hardsubTrackIndex = Just index }

transcodeAudio :: Transcode -> [String] -> Transcode
transcodeAudio t args = t { audioCodec = Recode args }

transcodeVideo :: Transcode -> [String] -> Transcode
transcodeVideo t args = t { videoCodec = Recode args }

isNoopTranscode Transcode { audioCodec = Copy, videoCodec = Copy, subtitlesCodec = Copy, hardsubTrackIndex = Nothing } = True
isNoopTranscode _ = False

transcodeCommand :: Transcode -> CommandTree
transcodeCommand t | isNoopTranscode t = (noopCommand . mediaFile . mediaInfo) t
transcodeCommand t = transcode `andCommand` (renameFile tempFile path)
  where
    path = (mediaFile . mediaInfo) t
    tempFile = (replaceExtension path (".tmp" ++ (takeExtension path)))

    prefix = ["-y", "-analyzeduration", "10000000", "-i", path]

    mapArg stream = ["-map", "0:" ++ stream ++ "?"]

    hardsubImgArg index = [
      "-filter_complex",
      ("[0:v][0:s:" ++ (show index) ++ "]overlay[v]"),
      "-map", "[v]"]

    hardsubTextArg index = (mapArg "v") ++ [
      "-vf", "subtitles=" ++ (quoteSource path) ++ ":si=" ++ (show index)]
      where
        quoteSource ('[':cs) = '\\':'[':(quoteSource cs)
        quoteSource (']':cs) = '\\':']':(quoteSource cs)
        quoteSource (c:cs)   = c:(quoteSource cs)
        quoteSource []       = []

    codecArg Copy stream = ["-c:" ++ stream, "copy"]
    codecArg (Recode args) stream = ["-c:" ++ stream] ++ args

    mapCopy stream = (mapArg stream) ++ (codecArg Copy stream)

    maybeHardsubRecode Nothing Nothing codec =
      (mapArg "v") ++ (codecArg codec "v") ++ (mapCopy "s")
    maybeHardsubRecode (Just track) (Just index) codec =
      let hardsub = if isTextSubtitle (trackFormat track)
                      then hardsubTextArg index
                      else hardsubImgArg index
       in case codec of
         Copy -> hardsub
         _    -> hardsub ++ (codecArg codec "v")

    transcode =
      shellCommand "ffmpeg" (
        prefix ++
        (maybeHardsubRecode (hardsubTrack t) (hardsubTrackIndex t) (videoCodec t)) ++
        (concatMap mapCopy ["d", "t"]) ++
        (mapArg "a") ++ (codecArg (audioCodec t) "a") ++
        [tempFile]
      )

renameFile :: String -> String -> CommandTree
renameFile from to = shellCommand "mv" [from, to]
