module Main where

import History

import Data.Trie

import Lib

import ConsolePrompt

main :: IO ()
main = do
    history <- initialHistory
    print history
    repl history
    where
        initialHistory :: IO HistoryTrie
        initialHistory = do
            raw <- readFile historyPath
            return (foldl updateHistory empty (lines raw))
