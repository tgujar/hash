module Main where

import History

import Data.Trie

import Lib

import ConsolePrompt
import Types

main :: IO ()
main = do
    history <- initialHistory
    print history
    repl (history, WS initStore [])
    where
        initialHistory :: IO HistoryTrie
        initialHistory = do
            raw <- readFile historyPath
            return (foldl updateHistory empty (lines raw))
