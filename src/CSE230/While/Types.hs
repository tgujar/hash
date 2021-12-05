module CSE230.While.Types where

import qualified Data.Map as M -- (findWithDefault, insert, empty)

--------------------------------------------------------------------------------
-- | Representing Programs 
--------------------------------------------------------------------------------

{- Programs in the language are simply values of the type
   where expressions are variables, constants or 
   binary operators applied to sub-expressions -}

data Statement 
   = Assign   Variable   Expression           -- ^ x = e
   | If       Expression Statement Statement  -- ^ if (e) {s1} else {s2}
   | While    Expression Statement            -- ^ while (e) {s}
   | Sequence Statement  Statement            -- ^ s1; s2
   | Skip                                     -- ^ no-op
   deriving (Eq, Show)
{-
ZACK:
   | Cd    Expression
   | Pwd 
   | Ls
   | Echo  Expression
-}


--------------------------------------------------------------------------------
-- | Representing Expressions 
--------------------------------------------------------------------------------

data Expression 
   = Var Variable                        -- ^ Variables x
   | Val Value                           -- ^ Constant Values v 
   | Op  Bop Expression Expression       -- ^ binary operators
   deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Representing Variables
--------------------------------------------------------------------------------

type Variable = String

--------------------------------------------------------------------------------
-- | Primitive Built-in Values 
--------------------------------------------------------------------------------

data Value 
   = IntVal Int
   | BoolVal Bool
   deriving (Eq, Show)
{-
ZACK:
   | StrVal String
-}


--------------------------------------------------------------------------------
-- | Binary Operators are 2-ary functions 
--------------------------------------------------------------------------------

data Bop 
   = Plus                                -- ^ (+)  :: Int  -> Int  -> Int
   | Minus                               -- ^ (-)  :: Int  -> Int  -> Int
   | Times                               -- ^ (*)  :: Int  -> Int  -> Int
   | Divide                              -- ^ (/)  :: Int  -> Int  -> Int
   | Gt                                  -- ^ (>)  :: Int -> Int -> Bool 
   | Ge                                  -- ^ (>=) :: Int -> Int -> Bool
   | Lt                                  -- ^ (<)  :: Int -> Int -> Bool
   | Le                                  -- ^ (<=) :: Int -> Int -> Bool
   deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Some example Statements that you can use to test your code 
--------------------------------------------------------------------------------
-- see "test/in/test.imp"
w_test :: Statement
w_test = (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) 
         `Sequence` 
         ((Assign "Y" (Val (IntVal 0))) 
          `Sequence` 
          (While (Op Gt (Var "X") (Val (IntVal 0))) 
               ((Assign "Y" (Op Plus (Var "Y") (Var "X"))) 
                `Sequence` 
                (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))))

-- see "test/in/fact.imp"
w_fact :: Statement
w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- see "test/in/abs.imp"
w_abs :: Statement
w_abs = (Sequence (Assign "X" (Op Minus (Val (IntVal 0)) (Val (IntVal 3)))) (If (Op Lt (Var "X") (Val (IntVal 0))) (Assign "X" (Op Minus (Val (IntVal 0)) (Var "X"))) Skip))

-- see "test/in/times.imp"
w_times :: Statement
w_times = (Sequence (Assign "X" (Val (IntVal 10))) (Sequence (Assign "Y" (Val (IntVal 3))) (Sequence (Assign "Z" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Z" (Op Plus (Var "Z") (Var "Y"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))))))
--------------------------------------------------------------------------------
-- | Representing the *Store* i.e. the machine's memory, as an associative
--   map from `Variable` to `Value` 
--------------------------------------------------------------------------------

type Store = M.Map Variable Value

