{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds #-}

module Eval where

import qualified Data.Map as M
import Data.List as L
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Identity
import           Types
import Data.Either
import Text.Parsec.String
import Parse as P
import System.Process (callProcess)
import Control.Exception
import System.Directory


-- ----------------------------------------------------------------------------------------------
-- -- | A Combined monad that is BOTH 
-- --    (i) a WState-Transformer monad 
-- --    (ii) an Exception monad with exceptions of type Value 
-- ----------------------------------------------------------------------------------------------
type MonadWhile m = (MonadIO m, MonadState WState m, MonadError Value m)

-- This returns the value of the variable from the nearest scope or parent scope
stackLookUp :: Variable -> Store -> Maybe Value
stackLookUp v [] = Nothing
stackLookUp v [m] =  M.lookup v m
stackLookUp v m = case v' of
                    Nothing -> stackLookUp v (tail m)
                    Just value -> v'
  where v' = M.lookup v (head m)

getScope :: RefScope -> Store -> ScopeVars
getScope Local stor  = head stor
getScope _ stor = last stor

pushScope :: (MonadWhile m) => m ()
pushScope  = do
  WS s log path <- get
  put (WS (initScope : s) log path)

popScope :: (MonadWhile m) => m ()
popScope  = do
  WS s log path <- get
  put (WS (tail s) log path)

----------------------------------------------------------------------------------------------
-- | `readVar x` returns the value of the variable `x` in the "current store"
----------------------------------------------------------------------------------------------
readVar :: (MonadWhile m) => Variable -> m Value
readVar x = do
  WS s _ _ <- get
  case stackLookUp x s of
    Just v  -> return v
    Nothing -> throwError $ error $ "Variable " ++ show x ++ " not found"

----------------------------------------------------------------------------------------------
-- | `writeVar x v` updates the value of `x` in the store to `v`
----------------------------------------------------------------------------------------------
writeVar :: (MonadState WState m) => RefScope -> Variable -> Value -> m ()
writeVar scope x v = do
  WS s log path <- get
  let s' = M.insert x v (getScope scope s)
  case scope of
    Local -> put (WS (s':tail s) log path)
    _ -> put (WS (init s ++ [s']) log path)

----------------------------------------------------------------------------------------------
-- | `printString msg` adds the message `msg` to the output log
----------------------------------------------------------------------------------------------
printString :: (MonadState WState m) => String -> m ()
printString msg = do
  WS s log path <- get
  put (WS s (msg:log) path)


-- TODO complete prefix operations
eval :: (MonadWhile m) => Expression -> m Value
eval (Var v)      = readVar v
eval (Val v)      = return v
eval (Op op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  semantics op v1 v2
eval (PrefixOp op e) = do
  v <- eval e
  prefSemantics op v


prefSemantics :: (MonadWhile m) => Prefop -> Value -> m Value
prefSemantics Not (BoolVal b) = return $ BoolVal (not b)
prefSemantics Not _ = throwError (StrVal "Invalid type for 'not' operation")
prefSemantics Neg (NumVal (Left a)) = return $ NumVal (Left $ negate a)
prefSemantics Neg (NumVal (Right a)) = return $ NumVal (Right $ negate a)
prefSemantics Neg _ = throwError (StrVal "Invalid type for 'negation' operation")

prefSemantics Pos (NumVal a) = return $ NumVal a
prefSemantics Pos _ = throwError (StrVal "Invalid type for 'negation' operation")


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

semantics And (BoolVal n1) (BoolVal n2) = return $ BoolVal (n1 && n2)
semantics Or (BoolVal n1) (BoolVal n2) = return $ BoolVal (n1 || n2)

semantics _ _ _ = throwError (StrVal "Types don't match")


-- isScopeFlag :: RefScope -> Bool
-- isScopeFlag f = case f of
--                     (Scope _) -> True
--                     _ -> False

-- TO DO: Functions and blocks arent being evaluated
evalS :: (MonadWhile m) => Statement -> m ()
evalS (Assign v f e) = do
  val <- eval e
  writeVar f v val
  return ()

evalS (If e s1 s2) = do
  val <- eval e
  case val of
    (BoolVal True) -> do{pushScope; evalS s1; popScope}
    (BoolVal False) -> do{pushScope; evalS s2; popScope}
    _ -> throwError (StrVal "Type error")


evalS (While e s) = do
  val <- eval e
  case val of
    (BoolVal True) -> do {pushScope; evalS s; evalS (While e s); popScope}
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

evalS (Block s1) = do
  pushScope
  evalS s1
  popScope

evalS Error = return ()

evalS (HashFile f) = do
  check <- liftIO $ doesFileExist f
  if check then liftIO $ runFile f else throwError $ StrVal "No such file"

-- setFunction :: (MonadWhile m) => RefScope -> Variable -> m ()
-- setFunction sc v = do
--   val <- readVar' sc v
--   liftIO $ print (show val)
--   printString $ show val

-- setFunction sc Erase v = do
--   WS s log <- get
--   let s' = M.delete v (getScope sc s)
--   case sc of
--     Local -> put (WS (s':tail s) log)
--     _ -> put (WS (init s ++ [s']) log)





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
runExec :: Exec a -> WState -> IO (Bool, WState)
runExec act s =  do
    r <- runExceptT (runStateT act s)
    case r of
      Left  v       -> do {print $ "Error:" ++ show v; return (False, s)}
      Right (a, s') -> return (True, s')

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing

runCmd :: String -> WState -> IO (Bool, WState)
runCmd str state = do
  p <- parseFromStringIO P.stmtParser str
  case p of
    Left err   -> do {print err; return (False, state)}
    Right stmt -> runExec (evalS stmt) state

-- >>> runCmd "echo 1+1" (WS initStore [] "")
-- "2"
-- True
--

-- Run a Hash script
runFile :: FilePath -> IO ()
runFile s = do
  p <- parseFromFile P.stmtParser s
  case p of
    Left err   -> print err
    Right stmt -> do {runExec (evalS stmt) (WS initStore [] ""); return ()}


-- function to debug parsing
printParsed :: FilePath -> IO ()
printParsed s = do
  p <- parseFromFile P.stmtParser s
  print p

-- >>> printParsed "test/test.hash"
-- Right (Sequence (Assign "X" Local (Val 10)) (Sequence (Assign "Y" Local (Val 3)) (Sequence (Assign "Z" Local (Val 0)) (Sequence (While (Op Gt (Var "X") (Val 0)) (Sequence (Assign "Z" Local (Val 3)) (Sequence (Print (Val "Hello world")) (Sequence (HashFile "/Users/tgujar/Projects/UCSD/CSE230/hash copy/test/test2.hash") (Assign "X" Global (Op Minus (Var "X") (Val 1))))))) (Print (Var "X"))))))
--

-- >>> runFile "test/test.hash"
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "\"Hello world\""
-- "0"
--

-- *** unit tests for runCmd and runFile ***

-- manually examine output of runCmd to see that it lists out the directory contents correctly
-- >>> runCmd "ls" (WS initStore [] "")
-- ChangeLog.md
-- LICENSE
-- README.md
-- Setup.hs
-- app
-- dist
-- hash.cabal
-- package.yaml
-- src
-- stack.yaml
-- stack.yaml.lock
-- test
--

listsEq :: Eq a => [a] -> [a] -> Bool
listsEq [] [] = True
listsEq [] _ = False
listsEq _ [] = False
listsEq (x:xs) (y:ys) = (x == y) && listsEq xs ys

-- runs the given command and checks that the resulting eval state is as expected
runCmdCheckStore :: String -> WState -> WState -> IO Bool
runCmdCheckStore cmd initialState (WS expectedStore expectedLog expectedPath) = do
  (b, WS store log path) <- runCmd cmd initialState
  let
    -- res = listsEq store expectedStore && listsEq log expectedLog && (path == expectedPath)
    res1 = listsEq store expectedStore
    res2 = listsEq log expectedLog
    res3 = path == expectedPath
    res = res1 && res2 && res3
  -- print res1
  -- print res2
  -- print res3
  return res

-- >>> runCmdCheckStore "set cat 5" (WS initStore [] "") (WS [M.fromList [("cat", NumVal (Left 5))]] [] "")
-- True
--

-- >>> runCmdCheckStore "set cat 5; echo $cat" (WS initStore [] "") (WS [M.fromList [("cat", NumVal (Left 5))]] ["5"] "")
-- "5"
-- True
--

-- >>> runCmdCheckStore "hash \"test/test.hash\"" (WS initStore [] "") (WS [M.fromList []] [] "")