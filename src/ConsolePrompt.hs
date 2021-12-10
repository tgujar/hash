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
                    -- (_, st@(WS _ _ path)) <- lift get
                    -- outputStrLn $ "Input was: " ++ input-- ++ "; current directory is " ++ path
                    (res, st') <- liftIO (runCmd cleanedInput st) -- runCmd and runFile are parallel functions; ideally we have something that can funnel the call to the right place
                    outputStrLn $ (show res)
                    Control.Monad.when res $
                        do
                            lift $ modify (\(history, _) -> (updateHistory history cleanedInput, st')) -- update history trie and state store
                            modifyHistory (addHistory cleanedInput)
                    loop

-- need to strip whitespace from the ends of input; otherwise we get weird double entries
-- e.g. Data.Trie.fromList [("gains",1),("gains ",1),("ls",1),("quit",6),("science",2),("sciencetest",1),("sciencetest ",1),("test",2),("test2",1),("this is a test",1)]
-- actually hm this seems like it's kind of a Haskeline problem
-- because it gets written out to the file with the extra space on the end
-- I will ignore this for now