{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

module History (findMatches, updateHistory, HistoryTrie) where

-- history as trie
-- https://hackage.haskell.org/package/bytestring-trie-0.2.6/docs/Data-Trie.html

import Data.Trie as Trie
import Data.Trie.Convenience as TrieConvenience

import qualified Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 as BLU
import Data.ByteString.UTF8 as BSU

import Control.Monad.State

import Control.Monad.Identity

import qualified Data.Bifunctor

type HistoryTrie = Trie Int

stringToByteString :: String -> BS.ByteString
stringToByteString input = BL.toStrict (BLU.fromString input)

findMatches :: HistoryTrie -> String -> [(String, Int)]
findMatches history input =
    let
        submap = Trie.submap (stringToByteString input) history
        matchesList = toList submap
    in
        Prelude.map (Data.Bifunctor.first BSU.toString) matchesList

updateHistory :: HistoryTrie -> String -> HistoryTrie
updateHistory history input = insertWith (+) (stringToByteString input) 1 history


-- some small tests --

testHistory :: HistoryTrie
testHistory = Trie.fromList (Prelude.map (Data.Bifunctor.first stringToByteString) [("ls -l", 1), ("ls -a", 2), ("ls", 3), ("mkdir blah", 1)])

-- >>> findMatches testHistory "l"
-- [("ls",3),("ls -a",2),("ls -l",1)]
--

-- >>> updateHistory testHistory "ls"
-- Data.Trie.fromList [("ls",4),("ls -a",2),("ls -l",1),("mkdir blah",1)]
--
