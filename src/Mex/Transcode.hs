module Mex.Transcode (
  Transcode,
  noopTranscode,
  isNoopTranscode,
  hardsubTrack,
  transcodeAudio,
  transcodeVideo,
  transcodeCommand,
) where

import System.FilePath (replaceExtension, takeExtension)

import Mex.CommandTree
import Mex.MediaInfo (MediaInfo, MediaTrack, trackIndex, mediaFile, trackId)

data Codec =
    Copy
  | Recode [String]
  deriving (Show)

data Transcode = Transcode {
  mediaInfo :: MediaInfo,
  audioCodec, videoCodec, subtitlesCodec :: Codec,
  hardsubTrackIndex :: Maybe Int
} deriving (Show)

noopTranscode :: MediaInfo -> Transcode
noopTranscode mediainfo =
  Transcode {
    mediaInfo = mediainfo,
    audioCodec = Copy,
    videoCodec = Copy,
    subtitlesCodec = Copy,
    hardsubTrackIndex = Nothing
  }

hardsubTrack :: Transcode -> MediaTrack -> Transcode
hardsubTrack transcode track =
  let Just index = trackIndex (mediaInfo transcode) "Text" (trackId track)
   in transcode { hardsubTrackIndex = Just index }

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

    hardsubArg index = [
      "-filter_complex",
      ("[0:v][0:s:" ++ (show index) ++ "]overlay[v]"),
      "-map", "[v]"]

    codecArg stream Copy = ["-c:" ++ stream, "copy"]
    codecArg stream (Recode args) = ["-c:" ++ stream] ++ args

    maybeHardsubRecode Nothing codec = (mapArg "v") ++ (codecArg "v" codec)
    maybeHardsubRecode (Just index) Copy = hardsubArg index
    maybeHardsubRecode (Just index) codec = (hardsubArg index) ++ (codecArg "v" codec)

    transcode =
      shellCommand "ffmpeg" (
        prefix ++
        (maybeHardsubRecode (hardsubTrackIndex t) (videoCodec t)) ++
        (concatMap mapArg ["a", "s", "d", "t"]) ++
        (concatMap ((flip codecArg) Copy) ["s", "d", "t"]) ++
        (codecArg "a" (audioCodec t)) ++
        [tempFile]
      )

renameFile :: String -> String -> CommandTree
renameFile from to = shellCommand "mv" [from, to]
