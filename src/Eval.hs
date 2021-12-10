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

----------------------------------------------------------------------------------------------
-- | `readVar x` returns the value of the variable `x` in the "current store"
----------------------------------------------------------------------------------------------
readVar :: (MonadWhile m) => Variable -> m Value
readVar x = do
  WS s _ <- get
  case Map.lookup x s of
    Just v  -> return v
    Nothing -> throwError $ error $ "Variable " ++ show x ++ " not found"

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


-- TODO complete prefix operations
eval :: (MonadWhile m) => Expression -> m Value
eval (Var v)      = readVar v
eval (Val v)      = return v
eval (Op op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  semantics op v1 v2

-- get the value from inside NumVal
getRL :: (Integral a, Num b) => (Either a b) -> b
getRL a = if isRight a then fromRight 0 a else (fromIntegral (fromLeft 0 a))

-- semantics for describing how operations work
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

-- TO DO: FUnctions and blocks arent being evaluated
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
  liftIO $ print (show val)
  printString $ show val

-- evalS (Error e) = do
--   val <- eval e
--   throwError val

evalS (External cmd args) = do
  liftIO $ helper cmd args
  return ()

helper :: FilePath -> [String] -> IO ()
helper cmd args= do
  res <- try (callProcess cmd args) :: IO (Either IOError ())
  case res of
      Left ex  ->  putStrLn $ "Caught exception: " ++ show ex
      Right val -> return ()    -- Use to debug:  putStrLn $ "The answer was: " ++ show val




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

-- Returns IO True if the operations succeeded IO False otherwise
runExec :: Exec a -> WState  -> IO Bool
runExec act s =  do
    r <- runExceptT (runStateT act s)
    case r of
      Left  v       -> do {print $ "Error:" ++ show v; return False}
      Right (a, s') -> return True

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing


-- Run a Hash script
runFile :: FilePath -> IO ()
runFile s = do
  p <- parseFromFile P.stmtParser s
  case p of
    Left err   -> print err
    Right stmt -> do {runExec (evalS stmt) (WS initStore []); return ()}


-- function to debug parsing
printParsed :: FilePath -> IO ()
printParsed s = do
  p <- parseFromFile P.stmtParser s
  print p

-- >>> printParsed "test/test.hash"
-- Right (Sequence (Assign "X" [Scope 'U'] (Val 10)) (Sequence (Assign "Y" [Scope 'U'] (Val 3)) (Sequence (Assign "Z" [Scope 'U'] (Val 0)) (While (Op Gt (Var "X") (Val 0)) (Sequence (Print (Val "Hello world")) (Sequence (External "ls" ["-la"]) (Assign "X" [Scope 'U'] (Op Minus (Var "X") (Val 1)))))))))
--

-- >>> runFile "test/test.hash"
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
-- "\"Hello world\""
-- total 100
-- drwxrwxr-x  7 cse230 cse230  4096 Dec  9 19:31 .
-- drwxr-xr-x 25 cse230 cse230  4096 Dec  9 12:12 ..
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 app
-- -rw-rw-r--  1 cse230 cse230    44 Dec  1 17:24 ChangeLog.md
-- drwxrwxr-x  8 cse230 cse230  4096 Dec  9 19:30 .git
-- -rw-rw-r--  1 cse230 cse230   250 Dec  9 19:30 .gitignore
-- -rw-rw-r--  1 cse230 cse230  2063 Dec  9 19:30 hash.cabal
-- -rw-rw-r--  1 cse230 cse230   118 Dec  9 19:30 .history
-- -rw-rw-r--  1 cse230 cse230 35149 Dec  1 17:24 LICENSE
-- -rw-rw-r--  1 cse230 cse230  1462 Dec  9 19:30 package.yaml
-- -rw-rw-r--  1 cse230 cse230  2074 Dec  1 17:24 README.md
-- -rw-rw-r--  1 cse230 cse230    46 Dec  1 17:24 Setup.hs
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:30 src
-- drwxrwxr-x  5 cse230 cse230  4096 Dec  9 19:31 .stack-work
-- -rw-rw-r--  1 cse230 cse230  2250 Dec  9 19:30 stack.yaml
-- -rw-------  1 cse230 cse230  1003 Dec  9 19:31 stack.yaml.lock
-- drwxrwxr-x  2 cse230 cse230  4096 Dec  9 19:05 test
--

