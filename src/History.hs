{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

module History (findMatches, updateHistory, HistoryTrie) where

-- history as trie
-- https://hackage.haskell.org/package/bytestring-trie-0.2.6/docs/Data-Trie.html

import qualified Data.Trie as Trie
import Data.Trie.Convenience as TrieConvenience

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU

import Control.Monad.State

import Control.Monad.Identity

import qualified Data.Bifunctor

import Test.QuickCheck

type HistoryTrie = Trie.Trie Int

stringToByteString :: String -> BS.ByteString
stringToByteString input = BL.toStrict (BLU.fromString input)

findMatches :: HistoryTrie -> String -> [(String, Int)]
findMatches history input =
    let
        submap = Trie.submap (stringToByteString input) history
        matchesList = Trie.toList submap
    in
        map (Data.Bifunctor.first BSU.toString) matchesList

updateHistory :: HistoryTrie -> String -> HistoryTrie
updateHistory history input = insertWith (+) (stringToByteString input) 1 history


-- some small tests --

testHistory :: HistoryTrie
testHistory = Trie.fromList (map (Data.Bifunctor.first stringToByteString) [("ls -l", 1), ("ls -a", 2), ("ls", 3), ("mkdir blah", 1)])

-- >>> findMatches testHistory "l"
-- [("ls",3),("ls -a",2),("ls -l",1)]
--

-- >>> updateHistory testHistory "ls"
-- Data.Trie.fromList [("ls",4),("ls -a",2),("ls -l",1),("mkdir blah",1)]
--

-- quickcheck --

arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

-- do updateHistory on every string
fromStrings :: [String] -> HistoryTrie
fromStrings = foldl updateHistory Trie.empty

-- HistoryTrie should be able to upsert a bunch of things and then have them all be findable
prop_upsert_vals :: Property
prop_upsert_vals = forAll (listOf arbitraryPrintableString) (\strings ->
    let
        trie = fromStrings strings
    in
        all (not . null . findMatches trie) strings -- this line got hella cleaned up by the hints thing
    )

-- >>> quickCheck prop_upsert_vals
-- +++ OK, passed 100 tests.
--

-- HistoryTrie upsert frequency values should be correct when compared to a frequency map

-- prop_upsert_freqs :: Property
-- prop_upsert_freqs = forAll (listOf arbitraryPrintableString) (\strings ->
--     let
--         freqMap = undefined
--         trie = fromStrings strings
--     in
--         undefined
--     )