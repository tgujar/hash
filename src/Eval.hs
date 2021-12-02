{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds #-}

module Eval where

import qualified Data.Map as Map
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Identity
import           Types

----------------------------------------------------------------------------------------------
-- | A Combined monad that is BOTH 
--    (i) a WState-Transformer monad 
--    (ii) an Exception monad with exceptions of type Value 
----------------------------------------------------------------------------------------------
type MonadWhile m = (MonadState WState m, MonadError Value m)

----------------------------------------------------------------------------------------------
-- | `readVar x` returns the value of the variable `x` in the "current store"
----------------------------------------------------------------------------------------------
readVar :: (MonadWhile m) => Variable -> m Value
readVar x = do 
  WS s _ <- get 
  case Map.lookup x s of
    Just v  -> return v
    Nothing -> throwError (IntVal 0)

----------------------------------------------------------------------------------------------
-- | `writeVar x v` updates the value of `x` in the store to `v`
----------------------------------------------------------------------------------------------
writeVar :: (MonadState WState m) => Variable -> Value -> m ()
writeVar x v = do 
  WS s log <- get 
  let s' = Map.insert x v s
  put (WS s' log)

----------------------------------------------------------------------------------------------
-- | `printString msg` adds the message `msg` to the output log
----------------------------------------------------------------------------------------------
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

-- class Operations a where
--   (+), (-), (==), (<=), (>=), (<), (>), div, (/), (*) :: Value -> Value -> m Value


-- instance Operations (IntVal a) where
--   div (IntVal v1) (IntVal v2) = if v2 P.== 0 then throwError (IntVal 1) else return $ IntVal (v1 `P.div` v2)
-- class Semantics a where
--   semantics :: (MonadWhile m) => Bop -> a -> a -> m a

-- instance Semantics Value where
--   semantics Divide (IntVal v1) (IntVal v2) = if v2 == 0 then throwError (IntVal 1) else return $ IntVal (v1 `div` v2)
--   semantics op v1@(IntVal _) v2@(IntVal _) = semantics op v1 v2
  
-- TODO: Write better error messages
-- semantics :: (MonadWhile m) => Bop -> Value -> Value -> m Value
-- semantics Plus v1@(IntVal a) v2@(IntVal b)  = intOp (+) v1 v2
-- semantics Plus v1@(FloatVal a) v2@(FloatVal b)  = floatOp (+) v1 v2
-- semantics Plus v1@(StrVal a) v2@(StrVal b)  = strOp (++) v1 v2

-- semantics Times v1@(IntVal a) v2@(IntVal b) = intOp (*) v1 v2
-- semantics Times v1@(FloatVal a) v2@(FloatVal b) = floatOp (*) v1 v2
-- -- semantics Times v1@(StrVal a) v2@(IntVal b) = strOp (foldr (++) "" $ replicate ) v1 v2

-- semantics Minus v1@(IntVal a) v2@(IntVal b) = intOp (-) v1 v2
-- semantics Minus v1@(FloatVal a) v2@(FloatVal b) = floatOp (-) v1 v2

-- semantics Divide v1@(IntVal a) v2@(IntVal b) = intOp (div) v1 v2
-- semantics Divide v1@(FloatVal a) v2@(FloatVal b) = floatOp (/) v1 v2


-- semantics Gt (IntVal v1) (IntVal v2)  = return $ BoolVal (v1 > v2)
-- semantics Ge (IntVal v1) (IntVal v2)  = return $ BoolVal (v1 >= v2)
-- semantics Lt (IntVal v1) (IntVal v2)  = return $ BoolVal (v1 < v2)
-- semantics Le (IntVal v1) (IntVal v2)  = return $ BoolVal (v1 <= v2)
-- semantics IsEq  (IntVal v1) (IntVal v2)  = return $ BoolVal (v1 == v2)

-- semantics Gt (FloatVal v1) (FloatVal v2)  = return $ BoolVal (v1 > v2)
-- semantics Ge (FloatVal v1) (FloatVal v2)  = return $ BoolVal (v1 >= v2)
-- semantics Lt (FloatVal v1) (FloatVal v2)  = return $ BoolVal (v1 < v2)
-- semantics Le (FloatVal v1) (FloatVal v2)  = return $ BoolVal (v1 <= v2)
-- semantics IsEq (FloatVal v1) (FloatVal v2)  = return $ BoolVal (v1 == v2)

-- semantics Gt (IntVal v1) (FloatVal v2)  = return $ BoolVal (fromIntegral v1 > v2)
-- semantics Ge (IntVal v1) (FloatVal v2)  = return $ BoolVal (fromIntegral v1 >= v2)
-- semantics Lt (IntVal v1) (FloatVal v2)  = return $ BoolVal (fromIntegral v1 < v2)
-- semantics Le (IntVal v1) (FloatVal v2)  = return $ BoolVal (fromIntegral v1 <= v2)


-- semantics Gt (FloatVal v1) (IntVal v2)  = return $ BoolVal (v1 >  fromIntegral v2)
-- semantics Ge (FloatVal v1) (IntVal v2)  = return $ BoolVal (v1 >= fromIntegral  v2)
-- semantics Lt (FloatVal v1) (IntVal v2)  = return $ BoolVal (v1 <  fromIntegral v2)
-- semantics Le (FloatVal v1) (IntVal v2)  = return $ BoolVal (v1 <= fromIntegral  v2)

-- semantics Gt (StrVal v1) (StrVal v2)  = return $ BoolVal (v1 > v2)
-- semantics Ge (StrVal v1) (StrVal v2)  = return $ BoolVal (v1 >= v2)
-- semantics Lt (StrVal v1) (StrVal v2)  = return $ BoolVal (v1 < v2)
-- semantics Le (StrVal v1) (StrVal v2)  = return $ BoolVal (v1 <= v2)
-- semantics IsEq (StrVal v1) (StrVal v2)  = return $ BoolVal (v1 == v2)

-- semantics _ _ _ = throwError (StrVal "Types don't match")

-- -- Take values out of monads and then check for division errors
-- intOp :: (MonadWhile m) => (Int -> Int -> Int) -> Value -> Value -> m Value
-- intOp div (IntVal v1) (IntVal v2) = if v2 == 0 then throwError (StrVal "Divide by zero") else return $ IntVal (v1 `div` v2)
-- intOp op (IntVal v1) (IntVal v2) = return $ IntVal (v1 `op` v2)
-- intOp _ _ _ = throwError (StrVal "Types don't match")

-- floatOp :: (MonadWhile m) => (Float -> Float -> Float) -> Value -> Value -> m Value
-- floatOp op (FloatVal v1) (FloatVal v2) = return $ FloatVal (v1 `op` v2)
-- floatOp _ _ _ = throwError (StrVal "Types don't match")

-- strOp :: (MonadWhile m) => (String -> String -> String) -> Value -> Value -> m Value
-- strOp op (StrVal v1) (StrVal v2) = return $ StrVal (v1 `op` v2) 
-- strOp _ _ _ = throwError (StrVal "Types don't match")



evalS :: (MonadWhile m) => Statement -> m ()
evalS (Assign v e) = do
  val <- eval e
  writeVar v val

evalS (If e s1 s2) = do
  val <- eval e
  case val of
    (BoolVal True) -> evalS s1
    (BoolVal False) -> evalS s2
    _ -> throwError (IntVal 2)

evalS (While e s) = do
  val <- eval e
  case val of
    (BoolVal True) -> do {evalS s; evalS (While e s)}
    (BoolVal False) -> return ()
    _               -> throwError (IntVal 2)      

evalS (Sequence s1 s2) = do
  evalS s1
  evalS s2

evalS Skip = return ()

evalS (Print e) = do
  val <- eval e
  printString $ show val
  

--------------------------------------------------------------------------
-- | Next, we will implement a *concrete instance* of a monad `m` that
--   satisfies the constraints of MonadWhile:
--------------------------------------------------------------------------

type Eval a = ExceptT Value (StateT WState (Identity)) a

--------------------------------------------------------------------------
-- | `runEval` implements a function to *run* the `Eval a` action from 
--   a starting `WState`. You can read the docs for `runState` and `runExceptT` 
--------------------------------------------------------------------------
runEval :: Eval a -> WState -> (Either Value a, WState)
runEval act s = runState (runExceptT act) s 

{- | `execute sto stmt` returns a triple `(sto', exn, log)` where
      * `st'` is the output state,
      * `exn` is (Just v) if the program terminates with an "uncaught" exception with Value v 
         or Nothing if the program terminates without an exception.
      * `log` is the log of messages generated by the `Print` statements.

-}
execute :: Store -> Statement -> (Store, Maybe Value, String)
execute sto stmt     = (sto', leftMaybe v, unlines (reverse log))
  where
    (v, WS sto' log) = runEval (evalS stmt) (WS sto [])

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing