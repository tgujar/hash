module Main where

import History

import Data.Trie

import Lib

import ConsolePrompt
import Types

import System.Directory

main :: IO ()
main = do
    history <- initialHistory
    print history
    currentDirectory <- getCurrentDirectory
    -- print currentDirectory
    repl (history, WS initStore [] currentDirectory)
    where
        initialHistory :: IO HistoryTrie
        initialHistory = do
            raw <- readFile historyPath
            return (foldl updateHistory empty (lines raw))
