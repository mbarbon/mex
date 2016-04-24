module Mex.Commands (
  mkvextractSubs,
  ffmpegExtractSubs,
  ffmpegConvertSubs,
  ffmpegConvertAllSubs,
  removeHtmlTags,
) where

import Control.Monad (foldM_)
import Data.List (intercalate)
import System.FilePath (replaceExtension, takeExtension)

import Mex.CommandTree
import Mex.MediaInfo

mkvextractSubs :: MediaInfo -> [MediaTrack] -> (CommandTree, [ExternalSubtitle])
mkvextractSubs mediainfo tracks =
  let parts = map mapping tracks
      command = shellCommand "mkvextract" (concat (prefix:(map fst parts)))
   in (command, map snd parts)
  where
    path = mediaFile mediainfo
    prefix = ["tracks", path]

    makeSub file format = ExternalSubtitle { subFormat = format, subFile = file }

    mapping :: MediaTrack -> ([String], ExternalSubtitle)
    mapping MediaTrack { trackId = trkId, trackFormat = format } =
      let index = (read trkId) - 1
          ext = trkId ++ "." ++ extForFormat format
          outFile = replaceExtension path ext
       in ([(show index) ++ ":" ++ outFile], makeSub outFile format)

ffmpegExtractSubs :: MediaInfo -> [MediaTrack] -> (CommandTree, [ExternalSubtitle])
ffmpegExtractSubs mediainfo tracks =
  let parts = mapping 0 tracks
      command = shellCommand "ffmpeg" (concat (prefix : (map fst parts)))
   in (command, map snd parts)
  where
    path = mediaFile mediainfo
    prefix = ["-y", "-i", path, "-c:s", "srt", "-an", "-vn"]

    makeSrt file = ExternalSubtitle { subFormat = MediaFormat "SRT", subFile = file }

    mapping :: Int -> [MediaTrack] -> [([String], ExternalSubtitle)]
    mapping _ []     = []
    mapping index (t:ts) =
      let idx = show index
          outFile = replaceExtension path (idx ++ ".srt")
          args = ["-map", "0:s:" ++ idx, outFile]
       in (args, makeSrt outFile):mapping (index + 1) ts

ffmpegConvertSubs :: ExternalSubtitle -> MediaFormat -> CommandTree
ffmpegConvertSubs es@ExternalSubtitle { subFormat = subFormat, subFile = source } format | subFormat == format = noopCommand source
ffmpegConvertSubs source format =
  let srcFile = (subFile source)
      outFile = replaceExtension srcFile (extForFormat format)
   in shellCommand "ffmpeg" ["-y", "-i", srcFile, outFile]

ffmpegConvertAllSubs :: [ExternalSubtitle] -> MediaFormat -> CommandTree
ffmpegConvertAllSubs subs format =
  foldl andCommand (noopCommand "This should never be seen") $ map (`ffmpegConvertSubs` format) subs

removeHtmlTags :: [ExternalSubtitle] -> CommandTree
removeHtmlTags subs =
  case (filter (isTextSubtitle . subFormat) subs) of
      []   -> noopCommand (intercalate ", " (map subFile subs))
      subs -> shellCommand "sed" (["-i.orig", "s/<[^>]\\+>//g"] ++ (map subFile subs))
