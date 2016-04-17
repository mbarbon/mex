module Mex.Scan (traverseWith) where

import Control.Monad (liftM, foldM, forM_)
import Data.List (partition)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))

traverseWith :: ([FilePath] -> IO a) -> FilePath -> IO ()
traverseWith action base = do
  entries <- liftM (filter interestingEntry) (getDirectoryContents base)
  (files, directories) <- partitionPaths base (return entries)
  action files
  forM_ directories $ \directory -> traverseWith action (base </> directory)
  where
    interestingEntry "."  = False
    interestingEntry ".." = False
    interestingEntry _    = True
    partitionPaths :: FilePath -> IO [FilePath] -> IO ([FilePath], [FilePath])
    partitionPaths base paths = do
      paths >>= (foldM (categorizePath base) ([], []))
    categorizePath :: FilePath -> ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
    categorizePath base (files, directories) path = do
      isFile <- doesFileExist (base </> path)
      return (if isFile
                then (path:files, directories)
                else (files, path:directories))
