module Mex.Commands (
  mkvextractSubs,
  ffmpegExtractSubs,
  ffmpegConvertSubs,
  ffmpegHardsub,
  removeHtmlTags,
  renameFile,
) where

import System.FilePath (replaceExtension, takeExtension)

import Mex.CommandTree (CommandTree, shellCommand, noopCommand, andCommand)
import Mex.MediaInfo

mkvextractSubs :: MediaInfo -> MediaTrack -> (CommandTree, ExternalSubtitle)
mkvextractSubs mediainfo track =
  let index = (read (trackId track)) - 1
      path = (mediaFile mediainfo)
      format = trackFormat track
      fileExt = extForFormat $ format
      outFile = replaceExtension path fileExt
      command = shellCommand "mkvextract" ["tracks", path, (show index) ++ ":" ++ outFile]
      result = ExternalSubtitle { subFormat = format, subFile = outFile }
   in (command, result)

ffmpegExtractSubs :: MediaInfo -> MediaTrack -> (CommandTree, ExternalSubtitle)
ffmpegExtractSubs mediainfo track =
  let Just index = trackIndex mediainfo "Text" (trackId track)
      path = (mediaFile mediainfo)
      outFile = replaceExtension path "srt"
      command = shellCommand "ffmpeg" ["-y", "-i", path, "-map", "0:s:" ++ (show index), "-an", "-vn", "-c:s", "srt", outFile]
      result = ExternalSubtitle { subFormat = MediaFormat "SRT", subFile = outFile }
   in (command, result)

ffmpegConvertSubs :: ExternalSubtitle -> MediaFormat -> CommandTree
ffmpegConvertSubs es@ExternalSubtitle { subFormat = subFormat, subFile = source } format | subFormat == format = noopCommand source
ffmpegConvertSubs source format =
  let srcFile = (subFile source)
      outFile = replaceExtension srcFile (extForFormat format)
   in shellCommand "ffmpeg" ["-y", "-i", srcFile, outFile]

ffmpegHardsub :: MediaInfo -> MediaTrack -> CommandTree
ffmpegHardsub mediainfo track =
  let Just index = trackIndex mediainfo "Text" (trackId track)
      path = (mediaFile mediainfo)
      tempFile = (replaceExtension path (".tmp" ++ (takeExtension path)))
      filterSpec = "[0:v][0:s:" ++ (show index) ++ "]overlay[v]"
      command = shellCommand "ffmpeg" ["-y", "-i", path, "-filter_complex", filterSpec, "-map", "[v]", "-map", "0:a", "-c:a", "copy", tempFile]
   in command `andCommand` (renameFile tempFile path)

removeHtmlTags :: ExternalSubtitle -> CommandTree
removeHtmlTags es | isTextSubtitle (subFormat es) =
  shellCommand "sed" ["-i.orig", "s/<[^>]\\+>//g", (subFile es)]
removeHtmlTags es = noopCommand (subFile es)

renameFile :: String -> String -> CommandTree
renameFile from to = shellCommand "mv" [from, to]
