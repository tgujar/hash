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

-- https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline.html
historySettings :: Settings (StateT HistoryTrie IO)
historySettings = Settings {
    complete = customComplete, -- why is it that when I change the completion function back to completeFilename, the history auto add starts working again?
    historyFile = Just historyPath,
    autoAddHistory = False -- should figure out how to turn this off and still make it work -> we only want to add successful commands to the history
}
-- autoAddHistory: ^ If 'True', each nonblank line returned by
-- @getInputLine@ will be automatically added to the history.

historyPath = ".history"

customComplete :: CompletionFunc (StateT HistoryTrie IO)
customComplete = completeWordWithPrev Nothing " \t" searchHistory

searchHistory :: String -> String -> StateT HistoryTrie IO [Completion]
searchHistory prefix suffix = do
    history <- get
    let
        matches = findMatches history (prefix++suffix) -- we aren't matching on tokens right now, only the whole line
    pure $ fmap (\s -> Completion s s True) (map fst matches)

repl :: HistoryTrie -> IO ()
repl initialHistory = flip evalStateT initialHistory $ runInputT historySettings loop
    where
        loop :: InputT (StateT HistoryTrie IO) ()
        loop = do
            minput <- getInputLine "% "
            case minput of
                Nothing -> return ()
                Just "quit" ->
                    do
                        modifyHistory (addHistory "quit")
                        outputStrLn "**Exited**"
                Just input -> do
                    outputStrLn $ "Input was: " ++ input ++ ";"
                    -- strip ending whitespace on input
                    let
                        cleanedInput = dropWhileEnd isSpace input
                    liftIO (runFile cleanedInput)
                    lift $ modify (`updateHistory` cleanedInput)
                    modifyHistory (addHistory cleanedInput)
                    loop

-- need to strip whitespace from the ends of input; otherwise we get weird double entries
-- e.g. Data.Trie.fromList [("gains",1),("gains ",1),("ls",1),("quit",6),("science",2),("sciencetest",1),("sciencetest ",1),("test",2),("test2",1),("this is a test",1)]
-- actually hm this seems like it's kind of a Haskeline problem
-- because it gets written out to the file with the extra space on the end
-- I will ignore this for now