module CSE230.While.Eval where

import qualified CSE230.While.Types as H
import qualified CSE230.While.Parse as P
import Data.Map
import Control.Monad.State hiding (when)

{- Build an evaluator for the WHILE Language using 
   the standard library's `State` [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
   to represent the world-transformer.

   Intuitively, `State s a` is equivalent to the world-transformer `s -> (a, s)`. 
   
   See the above documentation for more details. You can ignore the bits about `StateT` for now.
 -} 


-------------------------------------------------------------------------------
-- | Expression Evaluator 
-------------------------------------------------------------------------------
evalE :: H.Expression -> State H.Store H.Value
evalE e = do 
  s <- get
  return (eval s e) -- calculate the value with the state (map)

{- Fill in the implementation of `eval` that
   that takes as input an `Store` and an `Expression` 
   and returns a `Value`.
   
   HINT: The value `get` is of type `State Store Store`. Thus, to extract 
         the value of the "current store" in a variable `s` use `s <- get`.

   NOTE: we don't have exceptions yet, so if a variable is not found,
   simply return value `0`.

 -}

store0 :: H.Store
store0 = fromList [("X", H.IntVal 10),("Y", H.IntVal 20)]
-- >>> eval store0 (H.Var "X")
-- IntVal 10
--
-- >>> eval store0 (H.Var "Y")
-- IntVal 20
--
-- >>> eval store0 (H.Var "Z")
-- IntVal 0
--
-- >>> eval store0 (H.Val  (H.IntVal 92))
-- IntVal 92
--
-- >>> eval store0 (H.Val  (H.IntVal 150))
-- IntVal 150
--
-- >>> eval store0 (H.Var "X")
-- IntVal 10
--
-- >>> eval store0 (H.Op H.Plus  (H.Var "X") (H.Var "Y"))
-- IntVal 30
--
-- >>> eval store0 (H.Op H.Minus (H.Var "X") (H.Var "Y"))
-- IntVal (-10)
--
-- >>> eval store0 (H.Op H.Times (H.Var "X") (H.Var "Y"))
-- IntVal 200
--
-- >>> eval store0 (H.Op H.Divide (H.Var "Y") (H.Var "X"))
-- IntVal 2
--
-- >>> eval store0 (H.Op H.Divide (H.Var "X") (H.Var "Y"))
-- IntVal 0
--
-- >>> eval store0 (H.Op H.Divide (H.Var "X") (H.Val  (H.IntVal 0)))
-- *** Exception: divide by zero
-- IntVal 
--
-- >>> eval store0 (H.Op H.Gt (H.Var "Y") (H.Var "X"))
-- BoolVal True
--
-- >>> eval store0 (H.Op H.Ge (H.Var "Y") (H.Var "X"))
-- BoolVal True
--
-- >>> eval store0 (H.Op H.Lt (H.Var "Y") (H.Var "X"))
-- BoolVal False
--
-- >>> eval store0 (H.Op H.Le (H.Var "Y") (H.Var "X"))
-- BoolVal False
--

eval :: H.Store -> H.Expression -> H.Value  -- use the value in the map to calculate
eval s (H.Var x)      = if notMember x s  -- x not in s 
                          then H.IntVal 0  -- return 0
                          else s ! x  -- get the value of s[x]
eval _ (H.Val v)      = v
eval s (H.Op o e1 e2) = (semantics o) (eval s e1) (eval s e2)

semantics :: H.Bop -> H.Value -> H.Value -> H.Value
semantics H.Plus   = intOp  (+)
semantics H.Minus  = intOp  (-)
semantics H.Times  = intOp  (*)
semantics H.Divide = intOp  (div)
semantics H.Gt     = boolOp (>)
semantics H.Ge     = boolOp (>=)
semantics H.Lt     = boolOp (<)
semantics H.Le     = boolOp (<=)

intOp :: (Int -> Int -> Int) -> H.Value -> H.Value -> H.Value
intOp op (H.IntVal x) (H.IntVal y)  = H.IntVal (x `op` y)
intOp _  _            _             = H.IntVal 0

boolOp :: (Int -> Int -> Bool) -> H.Value -> H.Value -> H.Value
boolOp op (H.IntVal x) (H.IntVal y) = H.BoolVal (x `op` y)
boolOp _  _            _            = H.BoolVal False


-------------------------------------------------------------------------------
-- | Statement Evaluator 
-------------------------------------------------------------------------------

{- Fill in the definition of `evalS` that takes as input a `Statement` 
   and returns a world-transformer that returns a `()`. Here, the 
   world-transformer should in fact update the input store appropriately 
   with the assignments executed in the course of evaluating the `Statement`.

   HINT: The value `put` is of type `Store -> State Store ()`. 
   Thus, to "update" the value of the store with the new store `s'` 
   do `put s`.

   In the `If` case, if `e` evaluates to a non-boolean value, just skip both
   the branches. (We will convert it into a type error in the next homework.)
-}

evalS :: H.Statement -> State H.Store ()  -- evalS takes a statement to contruct a state transformer
evalS w@(H.While e s)    = do
  v <- evalE e  -- evaluate e to know it is true or false
  case v of
    (H.BoolVal True) -> do 
      evalS s
      evalS w  -- check the while condition again
    _ -> return ()  -- do nothing
evalS H.Skip             = do  -- take the state but doesn't change it at all
  return ()
evalS (H.Sequence s1 s2) = do  -- execute the first statement then the second statement
  evalS s1
  evalS s2
evalS (H.Assign x e )    = do
  e <- evalE e
  store <- get
  put (insert x e store)  -- update the state with a state that is modified
evalS (H.If e s1 s2)     = do
  v <- evalE e
  case v of
    (H.BoolVal True)  -> evalS s1
    (H.BoolVal False) -> evalS s2
    _                 -> return ()  -- simly skip both branches now
{- 
ZACK:

evalS (H.Cd e)           = do
  v <- evalE e
  case v of
    (H.StrVal _) -> evalS Assign 0 e
                    We can assign the address to a variable "0" in the store.
                    "0" is not a variable that can be used by a user because numbers are not allowed to be use
                    as a variable name in most languages. Thus, we can make sure this variable would not be
                    modified by a user.
    _            -> return (), do nothing since it's not a string, or throw an error 
evalS (H.Pwd)            = do
  evalS Assign 1 _
evalS (H.Ls)
  evalS Assign 2 _
evalS (H.Echo e)
  v <- evalE e
  case v of
    (H.StrVal _) -> evalS Assign 3 e
    _            -> return ()

Note1: We also need to modify the above Statement to make sure that these command line Statements do not appear
in a While, If, and Sequence.
Note2: After we evalute a statement, we should first check if the store contains keys of 0, 1, 2, or 3. If so,
we should run the turtle functions. Also, we need to remove these keys after we run the function.
-} 

-------------------------------------------------------------------------------
-- | Executor
-------------------------------------------------------------------------------

{- Fill in the implementation of `execS` such that `execS stmt store` 
   returns the new `Store` that results from evaluating the command 
   `stmt` from the world `store`. 

   HINT: You may want to use the library function from `Control.Monad.State` 

     execState :: State s a -> s -> s
 
 -}

-- >>> execS H.w_test empty
-- fromList [("X",IntVal 0),("Y",IntVal 10)]
--
-- >>> execS H.w_fact empty
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
--
-- >>> execS H.w_abs empty
-- fromList [("X",IntVal 3)]
--
-- >>> execS H.w_times empty
-- fromList [("X",IntVal 0),("Y",IntVal 3),("Z",IntVal 30)]
--



execS :: H.Statement -> H.Store -> H.Store
execS s = execState (evalS s)  -- discard the final value and return the final state

-------------------------------------------------------------------------------
-- | Running a Program 
-------------------------------------------------------------------------------

{- When you are done with the `execS`, the following function will 
   "run" a statement starting with the `empty` store (where no 
   variable is initialized). Running the program should print 
   the value of all variables at the end of execution.
 -}

-- >>> run H.w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]
--

run :: H.Statement -> IO ()
run stmt = do 
    putStrLn "Output Store:" 
    putStrLn (show (execS stmt empty))

-------------------------------------------------------------------------------
-- | Running a File
-------------------------------------------------------------------------------
runFile :: FilePath -> IO ()
runFile s = do 
  p <- P.parseFile s
  case p of
    Left err   -> print err
    Right stmt -> run stmt

printStore :: H.Store -> IO ()
printStore e = do 
  putStrLn "Environment:"
  putStrLn (show e)

-- When you are done you should see the following at the ghci prompt
--
-- >>> runFile "test/in/test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]
--

-- >>> runFile "test/in/fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
--

-- >>> runFile "test/in/times.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 3),("Z",IntVal 30)]
--

-- >>> runFile "test/in/abs.imp"
-- Output Store:
-- fromList [("X",IntVal 3)]
--

-- >>> runFile "test/in/zack.imp"
-- Output Store:
-- fromList [("X",IntVal 2)]
--
