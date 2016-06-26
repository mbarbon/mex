module Mex.CachedMediaInfo (
  cachedMediaInfo
) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, fileSize, modificationTime)

import Mex.MediaInfo

cachedMediaInfo :: FilePath -> IO MediaInfo
cachedMediaInfo path = do
  cacheKey <- fileKey path
  cachedInfo <- retrieveMediaInfo path cacheKey
  case cachedInfo of
    Just info -> return info
    Nothing   -> do
      freshInfo <- mediaInfo path
      storeMediaInfo path cacheKey freshInfo
      return freshInfo

retrieveMediaInfo :: FilePath -> String -> IO (Maybe MediaInfo)
retrieveMediaInfo path cacheKey = do
  conn <- makeConnection
  rows <- quickQuery' conn "SELECT serialized FROM MediaInfo WHERE path = ? AND key = ?" [toSql path, toSql cacheKey]
  disconnect conn
  if (length rows) /= 1
    then return Nothing
    else let serializedSql = (head . head) rows
             serialized = (fromSql serializedSql) :: String
          in return (Just ((read serialized) :: MediaInfo))

storeMediaInfo :: FilePath -> String -> MediaInfo -> IO ()
storeMediaInfo path cacheKey mediainfo = do
  conn <- makeConnection
  run conn "INSERT INTO MediaInfo (path, key, serialized) VALUES (?, ?, ?)"
               [toSql path, toSql cacheKey, toSql (show mediainfo)]
  run conn "DELETE FROM MediaInfo WHERE path = ? AND key <> ?"
               [toSql path, toSql cacheKey]
  commit conn
  disconnect conn
  return ()

makeConnection :: IO Connection
makeConnection = do
  homePath <- getEnv "HOME"
  createDirectoryIfMissing True (homePath </> ".mex")
  conn <- connectSqlite3 (homePath </> ".mex" </> "cache.db")
  run conn (unlines ["CREATE TABLE IF NOT EXISTS MediaInfo (",
                     "  id INTEGER PRIMARY KEY,",
                     "  path VARCHAR(1024) NOT NULL,",
                     "  key  VARCHAR(32) NOT NULL,",
                     "  serialized VARCHAR(1024) NOT NULL",
                     ")"]) []
  return conn

fileKey :: FilePath -> IO String
fileKey path = do
  fileStatus <- getFileStatus (path)
  let key = show (fileSize fileStatus, modificationTime fileStatus)
  return (show (md5 (pack key)))
