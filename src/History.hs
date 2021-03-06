{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

module History (findMatches, updateHistory, HistoryTrie, getSuffixDiff) where

-- history as trie
-- https://hackage.haskell.org/package/bytestring-trie-0.2.6/docs/Data-Trie.html

import qualified Data.Trie as Trie
import Data.Trie.Convenience as TrieConvenience

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU

import qualified Data.Map as M

import qualified Data.List as L

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
        raw = map (Data.Bifunctor.first BSU.toString) matchesList
    in
        L.sort raw

updateHistory :: HistoryTrie -> String -> HistoryTrie
updateHistory history input = insertWith (+) (stringToByteString input) 1 history

getSuffixDiff :: String -> String -> String
getSuffixDiff str prefix =
    case (str, prefix) of
        ([], _) -> []
        (str, []) -> str
        (c:cs, p:ps) -> if c == p then getSuffixDiff cs ps else c:cs


-- some small tests --

testHistory :: HistoryTrie
testHistory = Trie.fromList (map (Data.Bifunctor.first stringToByteString) [("ls -l", 1), ("ls -a", 2), ("ls", 3), ("mkdir blah", 1)])

-- >>> findMatches testHistory "l"
-- [("ls",3),("ls -a",2),("ls -l",1)]
--

-- >>> updateHistory testHistory "ls"
-- Data.Trie.fromList [("ls",4),("ls -a",2),("ls -l",1),("mkdir blah",1)]
--

-- >>> findMatches testHistory "mk"
-- [("mkdir blah",1)]
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

makeFreqMap :: [String] -> M.Map String Int
makeFreqMap = foldl (\m s -> M.insertWith (+) s 1 m) M.empty

prop_upsert_freqs :: Property
prop_upsert_freqs = forAll (listOf arbitraryPrintableString) (\strings ->
    let
        freqMap = makeFreqMap strings
        trie = fromStrings strings
    in
        all (\s -> snd (head (findMatches trie s)) == (freqMap M.! s)) strings
    )

-- >>> quickCheck prop_upsert_freqs
-- +++ OK, passed 100 tests.
--
