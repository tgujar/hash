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
import System.Process (callProcess)
import Control.Exception
import Control.Arrow (Arrow(first))


-- ----------------------------------------------------------------------------------------------
-- -- | A Combined monad that is BOTH 
-- --    (i) a WState-Transformer monad 
-- --    (ii) an Exception monad with exceptions of type Value 
-- ----------------------------------------------------------------------------------------------
type MonadWhile m = (MonadIO m, MonadState WState m, MonadError Value m)

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
  return ()

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

evalS (External cmd args) = do
  liftIO $ helper cmd args
  return ()

helper :: FilePath -> [String] -> IO ()
helper cmd args= do
  res <- try (callProcess cmd args) :: IO (Either IOError ())
  case res of
      Left ex  ->  putStrLn $ "Caught exception: " ++ show ex
      Right val -> putStrLn $ "The answer was: " ++ show val

-- evalS (External cmg args) = do



-- --------------------------------------------------------------------------
-- -- | Next, we will implement a *concrete instance* of a monad `m` that
-- --   satisfies the constraints of MonadWhile:
-- --------------------------------------------------------------------------

-- type Eval = StateT WState (ExceptT Value IO)
type Eval = (StateT WState IO)
type Exec = (StateT WState (ExceptT Value IO))

-- --------------------------------------------------------------------------
-- -- | `runEval` implements a function to *run* the `Eval a` action from 
-- --   a starting `WState`. You can read the docs for `runState` and `runExceptT` 
-- --------------------------------------------------------------------------

runExec :: Exec a -> WState  -> IO ()
runExec act s =  do
    r <- runExceptT (runStateT act s)
    case r of
      Left  v       -> print v
      Right (a, s') -> print ()

-- runEval :: Eval a -> WState -> IO ()
-- runEval ev act =  do 
--   p <- 



-- {- | `execute sto stmt` returns a triple `(sto', exn, log)` where
--       * `st'` is the output state,
--       * `exn` is (Just v) if the program terminates with an "uncaught" exception with Value v 
--          or Nothing if the program terminates without an exception.
--       * `log` is the log of messages generated by the `Print` statements.

-- -}

-- execute :: Eval a 
-- execute = do
--   r <- runExecAndReset (execs ss) env
--   case r of
--     Left v  -> return v
--     Right() -> runtimeError "Function did not return anything"



-- execute :: Store -> Statement -> IO ()
-- execute sto (External cmd args) = do
--     res <- try (callProcess cmd args) :: IO (Either IOError ())
--     case res of
--         Left ex  ->  putStrLn $ "Caught exception: " ++ show ex
--         Right val -> putStrLn $ "The answer was: " ++ show val
-- execute sto stmt       = do
--   let (v, WS sto' log) = runEval (evalS stmt) (WS sto [])
--   run (sto', leftMaybe v, unlines (reverse log))


leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing

run :: (Store, Maybe Value, String) -> IO ()
run (stor, err, log) = do
  print log
  case err of
    Nothing -> return ()
    Just va -> print err

runFile :: FilePath -> IO ()
runFile s = do
  p <- parseFromFile P.stmtParser s
  case p of
    Left err   -> print err
    Right stmt -> runExec (evalS stmt) (WS initStore [])

printParsed :: FilePath -> IO ()
printParsed s = do
  p <- parseFromFile P.stmtParser s
  print p

-- >>> printParsed "test/test.hash"
-- Right (Sequence (Assign "X" [Scope 'U'] (Val (NumVal (Left 10)))) (Sequence (Assign "Y" [Scope 'U'] (Val (NumVal (Left 3)))) (Sequence (Assign "Z" [Scope 'U'] (Val (NumVal (Left 0)))) (While (Op Gt (Var "X") (Val (NumVal (Left 0)))) (Sequence (Print (Val (StrVal "Hello world"))) (Assign "X" [Scope 'U'] (Op Minus (Var "X") (Val (NumVal (Left 1))))))))))
--

-- >>> runFile "test/test.hash"
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- total 152
-- drwxr-xr-x  17 tgujar  staff    544 Dec  1 16:30 .
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 00:17 ..
-- -rw-r--r--@  1 tgujar  staff   6148 Dec  9 02:52 .DS_Store
-- drwxr-xr-x  15 tgujar  staff    480 Dec  9 14:58 .git
-- -rw-r--r--   1 tgujar  staff    241 Nov 29 02:02 .gitignore
-- drwxr-xr-x   7 tgujar  staff    224 Dec  9 14:53 .stack-work
-- -rw-r--r--   1 tgujar  staff     44 Nov 29 02:02 ChangeLog.md
-- -rw-r--r--   1 tgujar  staff  35149 Nov 29 02:02 LICENSE
-- -rw-r--r--   1 tgujar  staff   2074 Nov 29 02:02 README.md
-- -rw-r--r--   1 tgujar  staff     46 Nov 29 02:02 Setup.hs
-- drwxr-xr-x   3 tgujar  staff     96 Nov 29 02:02 app
-- -rw-r--r--   1 tgujar  staff   1630 Dec  9 04:01 hash.cabal
-- -rw-r--r--   1 tgujar  staff   1192 Dec  9 04:00 package.yaml
-- drwxr-xr-x   8 tgujar  staff    256 Dec  9 12:47 src
-- -rw-r--r--   1 tgujar  staff   2209 Nov 29 02:02 stack.yaml
-- -rw-------   1 tgujar  staff    541 Nov 29 02:04 stack.yaml.lock
-- drwxr-xr-x   4 tgujar  staff    128 Dec  9 02:52 test
-- The answer was: ()
-- ()
--

