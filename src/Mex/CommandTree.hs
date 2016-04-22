module Mex.CommandTree (
  CommandTree,
  shellCommand,
  noopCommand,
  noCommand,
  andCommand,
  orCommand,
  runVerbose,
  viableCommand,
) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 (putStrLn)

import Control.Monad (foldM)
import Data.List (intercalate)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process.ByteString (readProcessWithExitCode)

data CommandOperation = And | Or deriving (Eq, Read, Show)
data CommandTree = CommandChain CommandOperation [CommandTree]
                 | Command String [String]
                 | Noop FilePath
                 | None FilePath
  deriving (Eq, Read, Show)

shellCommand :: String -> [String] -> CommandTree
shellCommand command args = Command command args

concatCommand :: CommandOperation -> CommandTree -> CommandTree -> CommandTree
concatCommand op none@(None _) cmd2 = none
concatCommand op cmd1 none@(None _) = none
concatCommand op (Noop _) cmd2 = cmd2
concatCommand op cmd1 (Noop _) = cmd1
concatCommand op cmd1 cmd2 = CommandChain op (cmd1 : cmd2 : [])

andCommand = concatCommand And
orCommand = concatCommand Or
noopCommand = Noop
noCommand = None

displayCommand :: CommandTree -> String
displayCommand (None name) = "# can't do anything for " ++ name
displayCommand (Noop name) = "# nothing to do for " ++ name
displayCommand (Command command args) =
  command ++ " '" ++ (intercalate "' '" args) ++ "'"
displayCommand (CommandChain And commands) =
  intercalate " && \\\n    " (map displayCommand commands)
displayCommand (CommandChain Or commands) =
  intercalate " || \\\n    " (map displayCommand commands)

runCommand :: CommandTree -> IO (Bool, ByteString.ByteString)
runCommand (None _) = return (True, ByteString.empty)
runCommand (Noop _) = return (True, ByteString.empty)
runCommand (Command command args) =
  do (exitCode, output, error) <- readProcessWithExitCode command args ByteString.empty
     case exitCode of
       ExitSuccess   -> return (True, output)
       ExitFailure _ -> return (False, if ByteString.null error then output else error)
runCommand (CommandChain And commands) =
  foldM mergeResult (True, ByteString.empty) commands
  where
    mergeResult fail@(False, _) _ = return fail
    mergeResult _ command = runCommand command
runCommand (CommandChain Or commands) =
  foldM mergeResult (False, ByteString.empty) commands
  where
    mergeResult success@(True, _) _ = return success
    mergeResult _ command = runCommand command

runVerbose :: CommandTree -> IO (Bool, ByteString.ByteString)
runVerbose command =
  do putStrLn (displayCommand command)
     (success, output) <- runCommand command
     if success
       then return ()
       else putStr "Command failed:\n\n" >> Data.ByteString.Char8.putStrLn output
     return (success, output)

viableCommand :: [CommandTree] -> CommandTree
viableCommand = foldl pickBest (None "This should never be seen")
  where
    pickBest (None _) cmd2 = cmd2
    pickBest cmd1 (None _) = cmd1
    pickBest noop@(Noop _) cmd2 = noop
    pickBest cmd1 noop@(Noop _) = noop
    -- arbitrary
    pickBest cmd1 cmd2 = cmd1
