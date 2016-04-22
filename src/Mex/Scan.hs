module Mex.Scan (
  traverseWith,
) where

import Control.Monad (liftM, foldM, forM_)
import Data.List (partition)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath (makeRelative, (</>))

traverseWith :: ([FilePath] -> IO a) -> FilePath -> IO ()
traverseWith action base = traverseWith_ action base base

traverseWith_ action base current = do
  entries <- liftM (filter interestingEntry) (getDirectoryContents current)
  (files, directories) <- partitionPaths (return (map (current </>) entries))
  action $ map (makeRelative base) files
  forM_ directories $ \directory -> traverseWith_ action base (base </> directory)
  where
    interestingEntry "."  = False
    interestingEntry ".." = False
    interestingEntry _    = True
    partitionPaths :: IO [FilePath] -> IO ([FilePath], [FilePath])
    partitionPaths paths = do
      paths >>= (foldM categorizePath ([], []))
    categorizePath :: ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
    categorizePath (files, directories) path = do
      isFile <- doesFileExist path
      return (if isFile
                then (path:files, directories)
                else (files, path:directories))
