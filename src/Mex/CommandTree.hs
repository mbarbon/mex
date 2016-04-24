module Mex.CommandTree (
  CommandTree,
  FailureDescription,
  shellCommand,
  noopCommand,
  noCommand,
  internalCommand,
  andCommand,
  orCommand,
  runVerbose,
  displayCommand,
  viableCommand,
  isViableCommand,
  commandSuccess,
  describeFailure,
) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import Control.Monad (foldM)
import Data.List (intercalate)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process.ByteString (readProcessWithExitCode)

type FailureDescription = ByteString.ByteString

data CommandOperation = And | Or deriving (Eq, Read, Show)
data CommandTree = CommandChain CommandOperation [CommandTree]
                 | ShellCommand String [String]
                 | Internal String (IO (Bool, ByteString.ByteString))
                 | Noop FilePath
                 | None FilePath

shellCommand :: String -> [String] -> CommandTree
shellCommand command args = ShellCommand command args

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
internalCommand = Internal

displayCommand :: CommandTree -> String
displayCommand (None name) = "# can't do anything for " ++ name
displayCommand (Noop name) = "# nothing to do for " ++ name
displayCommand (ShellCommand command args) =
  command ++ " '" ++ (intercalate "' '" args) ++ "'"
displayCommand (Internal description _) = description
displayCommand (CommandChain And commands) =
  intercalate " && \\\n    " (map displayCommand commands)
displayCommand (CommandChain Or commands) =
  intercalate " || \\\n    " (map displayCommand commands)

runCommand :: CommandTree -> IO (Bool, ByteString.ByteString)
runCommand (None _) = return (True, ByteString.empty)
runCommand (Noop _) = return (True, ByteString.empty)
runCommand (ShellCommand command args) =
  do (exitCode, output, error) <- readProcessWithExitCode command args ByteString.empty
     case exitCode of
       ExitSuccess   -> return (True, output)
       ExitFailure _ -> return (False, if ByteString.null error then output else error)
runCommand (Internal _ action) = action
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
       else putStr "Command failed:\n\n" >> Char8.putStrLn output
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

isViableCommand (None _) = False
isViableCommand _ = True

describeFailure :: String -> FailureDescription
describeFailure = Char8.pack

commandSuccess :: IO (Bool, FailureDescription)
commandSuccess = return (True, describeFailure "")
