

module CSE230.Fold where

import qualified Data.List as L

libraryFoldl :: (b -> a -> b) -> b -> [a] -> b
libraryFoldl = L.foldl

libraryFoldr :: (a -> b -> b) -> b -> [a] -> b
libraryFoldr = foldr

-------------------------------------------------------------------------------
-- | Using the standard `L.foldl` define a list `reverse` function
-------------------------------------------------------------------------------

-- >>> myReverse [1,2,3,4,5]
-- [5,4,3,2,1]
--

myReverse :: [a] -> [a]
myReverse xs = L.foldl f b xs
  where
    f = (\ys y -> y : ys)  -- put the new element in front of the current list
    b = []  -- initial list


-------------------------------------------------------------------------------
-- | Define fold-right using `L.foldl`
-------------------------------------------------------------------------------

-- >>> L.foldr (-) 0 [1,2,3,4,5]
-- 3
--
-- >>> L.foldr (-) 0 [3,6,1,2,8]
-- 4
--

-- >>> myFoldr (-) 0 [1,2,3,4,5]
-- 3
--
-- >>> myFoldr (-) 0 [3,6,1,2,8]
-- 4
--
-- BECAUSE: (1 - (2 - (3 - (4 - (5 - 0)))))

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f b xs = libraryFoldl f' b' xs'
  where
    f'         = \x y -> (f y x)
    b'         = b
    xs'        = xs


-------------------------------------------------------------------------------
-- | Define fold-left using `L.foldr`
-------------------------------------------------------------------------------
-- >>> myFoldl (-) 0 [1,2,3,4,5]
-- -15
--

-- >>> L.foldl (-) 0 [8,8,1,2,4]
-- -23
--
-- >>> myFoldl (-) 0 [8,8,1,2,4]
-- -23
--

-- BECAUSE ((((0 - 1) - 2)  - 3) - 4) - 5

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b xs = libraryFoldr f' b' xs'
  where
    f'         = \x y -> (f y x)
    b'         = b
    xs'        = xs

-- | [Extra] Can you figure out why `whySoSlow` takes much longer than `whySoFast`?

whySoSlow :: () -> Integer
whySoSlow _ = L.foldl (+) 0 [1..1000000]

whySoFast :: () -> Integer
whySoFast _ = L.foldl' (+) 0 [1..1000000]
