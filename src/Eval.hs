{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds #-}

module Eval where

import qualified Data.Map as Map
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Identity
import           Types
import Data.Either
import Text.Parsec.String
import Parse as P
import Data.Map


-- ----------------------------------------------------------------------------------------------
-- -- | A Combined monad that is BOTH 
-- --    (i) a WState-Transformer monad 
-- --    (ii) an Exception monad with exceptions of type Value 
-- ----------------------------------------------------------------------------------------------
type MonadWhile m = (MonadState WState m, MonadError Value m)

-- ----------------------------------------------------------------------------------------------
-- -- | `readVar x` returns the value of the variable `x` in the "current store"
-- ----------------------------------------------------------------------------------------------
readVar :: (MonadWhile m) => Variable -> m Value
readVar x = do
  WS s _ <- get
  case Map.lookup x s of
    Just v  -> return v
    Nothing -> throwError $ error $ "Variable " ++ show x ++ " not found"

-- ----------------------------------------------------------------------------------------------
-- -- | `writeVar x v` updates the value of `x` in the store to `v`
-- ----------------------------------------------------------------------------------------------
writeVar :: (MonadState WState m) => Variable -> Value -> m ()
writeVar x v = do
  WS s log <- get
  let s' = Map.insert x v s
  put (WS s' log)

-- ----------------------------------------------------------------------------------------------
-- -- | `printString msg` adds the message `msg` to the output log
-- ----------------------------------------------------------------------------------------------
printString :: (MonadState WState m) => String -> m ()
printString msg = do
  WS s log <- get
  put (WS s (msg:log))


eval :: (MonadWhile m) => Expression -> m Value
eval (Var v)      = readVar v
eval (Val v)      = return v
eval (Op op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  semantics op v1 v2

getRL :: (Integral a, Num b) => (Either a b) -> b
getRL a = if isRight a then fromRight 0 a else (fromIntegral (fromLeft 0 a))

-- -- TODO: fix errors in type matching, Use FlexibleContexts?
semantics :: (MonadWhile m) => Bop -> Value -> Value -> m Value
semantics Plus (NumVal (Left n1)) (NumVal (Left n2)) = return $ NumVal (Left (n1 + n2))
semantics Plus (NumVal n1) (NumVal n2) = return $ NumVal (Right ((getRL n1) + (getRL n2)))
semantics Plus (StrVal s1) (StrVal s2) = return $ StrVal (s1 ++ s2)

semantics Minus (NumVal (Left n1)) (NumVal (Left n2)) = return $ NumVal (Left (n1 - n2))
semantics Minus (NumVal n1) (NumVal n2) = return $ NumVal (Right ((getRL n1) - (getRL n2)))

semantics Times (NumVal (Left n1)) (NumVal (Left n2)) = return $ NumVal (Left (n1 * n2))
semantics Times (NumVal n1) (NumVal n2) = return $ NumVal (Right ((getRL n1) * (getRL n2)))

semantics Divide (NumVal n1) (NumVal n2) = do
  let a = getRL n1
  let b = getRL n2
  if b == 0.0 then throwError (StrVal "Divide by zero") else return $ NumVal (Right (a / b))

semantics Gt (NumVal n1) (NumVal n2) = return $ BoolVal ((getRL n1) > (getRL n2))
semantics Gt (StrVal n1) (StrVal n2) = return $ BoolVal (n1 > n2)

semantics Ge (NumVal n1) (NumVal n2) = return $ BoolVal ((getRL n1) > (getRL n2))
semantics Ge (StrVal n1) (StrVal n2) = return $ BoolVal (n1 >= n2)


semantics Lt (NumVal n1) (NumVal n2) = return $ BoolVal ((getRL n1) < (getRL n2))
semantics Lt (StrVal n1) (StrVal n2) = return $ BoolVal (n1 < n2)

semantics Le (NumVal n1) (NumVal n2) = return $ BoolVal ((getRL n1) < (getRL n2))
semantics Le (StrVal n1) (StrVal n2) = return $ BoolVal (n1 <= n2)
semantics _ _ _ = throwError (StrVal "Types don't match")


evalS :: (MonadWhile m) => Statement -> m ()
evalS (Assign v _ e) = do
  val <- eval e
  writeVar v val

evalS (If e s1 s2) = do
  val <- eval e
  case val of
    (BoolVal True) -> evalS s1
    (BoolVal False) -> evalS s2
    _ -> throwError (StrVal "Type error")

evalS (While e s) = do
  val <- eval e
  case val of
    (BoolVal True) -> do {evalS s; evalS (While e s)}
    (BoolVal False) -> return ()
    _               -> throwError (StrVal "Type error")

evalS (Sequence s1 s2) = do
  evalS s1
  evalS s2

evalS Skip = return ()

evalS (Print e) = do
  val <- eval e
  printString $ show val


-- --------------------------------------------------------------------------
-- -- | Next, we will implement a *concrete instance* of a monad `m` that
-- --   satisfies the constraints of MonadWhile:
-- --------------------------------------------------------------------------

type Eval a = ExceptT Value (StateT WState (Identity)) a

-- --------------------------------------------------------------------------
-- -- | `runEval` implements a function to *run* the `Eval a` action from 
-- --   a starting `WState`. You can read the docs for `runState` and `runExceptT` 
-- --------------------------------------------------------------------------
runEval :: Eval a -> WState -> (Either Value a, WState)
runEval act s = runState (runExceptT act) s

-- {- | `execute sto stmt` returns a triple `(sto', exn, log)` where
--       * `st'` is the output state,
--       * `exn` is (Just v) if the program terminates with an "uncaught" exception with Value v 
--          or Nothing if the program terminates without an exception.
--       * `log` is the log of messages generated by the `Print` statements.

-- -}
execute :: Store -> Statement -> (Store, Maybe Value, String)
execute sto stmt     = (sto', leftMaybe v, unlines (reverse log))
  where
    (v, WS sto' log) = runEval (evalS stmt) (WS sto [])

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing

run :: (Store, Maybe Value, String) -> IO ()
run (stor, err, log) = do
    putStrLn "Output :"
    print stor
    print err
    print log

runFile :: FilePath -> IO ()
runFile s = do
  p <- parseFromFile P.stmtParser s
  case p of
    Left err   -> print err
    Right stmt -> run (execute Map.empty stmt)

-- >>> runFile "test/test.hash"
-- Output :
-- fromList [("X",NumVal (Left 0)),("Y",NumVal (Left 3)),("Z",NumVal (Left 0))]
-- Nothing
-- "StrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\nStrVal \"Hello world\"\n"
--

