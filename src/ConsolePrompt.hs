module ConsolePrompt (repl, historyPath) where

import System.Console.Haskeline

import History
import Control.Monad.IO.Class

import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.State (StateT (runStateT), evalStateT, get, modify)

import Data.Trie

import Eval
import System.Console.Haskeline.History (addHistory)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Types
import qualified Control.Monad

type REPLState = (HistoryTrie, WState)

-- https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline.html
historySettings :: Settings (StateT REPLState IO)
historySettings = Settings {
    complete = customComplete, -- why is it that when I change the completion function back to completeFilename, the history auto add starts working again?
    historyFile = Just historyPath,
    autoAddHistory = False -- should figure out how to turn this off and still make it work -> we only want to add successful commands to the history
}
-- autoAddHistory: ^ If 'True', each nonblank line returned by
-- @getInputLine@ will be automatically added to the history.

historyPath = ".history"

customComplete :: CompletionFunc (StateT REPLState IO)
customComplete = completeWordWithPrev Nothing " \t" searchHistory

searchHistory :: String -> String -> StateT REPLState IO [Completion]
searchHistory rev_prefix suffix = do
    (history, _) <- get
    let
        prefix = reverse rev_prefix
        matches = findMatches history (dropWhileEnd isSpace (prefix++suffix)) -- we aren't matching on tokens right now, only the whole line
    pure $ fmap (\s -> Completion s s True) (map (\(s, freq) -> getSuffixDiff s prefix) matches)

repl :: REPLState -> IO ()
repl initial = flip evalStateT initial $ runInputT historySettings loop
    where
        loop :: InputT (StateT REPLState IO) ()
        loop = do
            (_, st@(WS _ _ path)) <- lift get
            minput <- getInputLine (path ++ "# ")
            case minput of
                Nothing -> return ()
                Just "quit" ->
                    do
                        modifyHistory (addHistory "quit")
                        outputStrLn "**Exited**"
                Just input -> do
                    -- strip ending whitespace on input
                    let
                        cleanedInput = dropWhileEnd isSpace input
                    (res, st') <- liftIO (runCmd cleanedInput st)
                    outputStrLn $ show res
                    Control.Monad.when res $
                        do
                            lift $ modify (\(history, _) -> (updateHistory history cleanedInput, st')) -- update history trie and state store
                            modifyHistory (addHistory cleanedInput)
                    loop
