{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

module History (findMatches, updateHistory, replT, run) where

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

{-
This trie implementation is wack
I want to look up full strings given a prefix
But most of the functions seem to return prefixes given a full string
submap seems like the only usable one for my purposes
I also have no idea what the value would be; frequency I guess?
So we want upsert not insert

more fundamentally, I am having type issues with the monads
without monads, it seems decently straightfoward:
    input is history trie + prefix, output is list of keys that match the prefix sorted by frequency
-}

-- ok let's figure out the state monad stuff
-- if I can do that, then all that's left should be I/O

-- type SomeState m = MonadState HistoryTrie m

-- something :: (SomeState m) => String -> m [(String, Int)]
-- something input = do
--     history <- get
--     return (findMatches history input)

-- how does this work at the top level again?
{-
The way it worked with While+ was like...

top level:
execute store statement -> (store, error channel, print output)

this calls runEval (evalS statement) (initial store)

-}

type MyState a = StateT HistoryTrie IO a

replT :: MyState ()
replT = do
    str <- liftIO Prelude.getLine
    state <- get
    liftIO $ Prelude.putStrLn ("current state: " ++ show state)
    liftIO $ Prelude.putStrLn ("potential suggestions: " ++ show (findMatches state str))
    liftIO $ Prelude.putStrLn ("setting state: " ++ str)
    put (updateHistory state str)
    replT

-- for now just do `stack ghci` and `run`; then type things and you'll see the history update
run = runStateT replT Trie.empty