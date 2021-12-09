{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

import           Data.String
import           GHC.Exts
import           GHC.Generics
import qualified Data.Map as Map

type Variable = String

-- TO DO: Implement scoping
-- Store should be a stack of Maps, where bottom of stack represents the global scope
type Store    = Map.Map Variable Value


data Value
  = NumVal (Either Integer Double)
  | BoolVal Bool
  | StrVal String
  deriving (Eq, Generic, Show)

data Flag
  = Scope Char  -- flags for scope as defined here https://fishshell.com/docs/current/cmds/set.html?highlight=set
  | Operation Char
  deriving (Show)

data Expression
  = Var Variable
  | Val Value
  | Op  Bop Expression Expression
  | PrefixOp Prefop Expression
  deriving (Show)

data Prefop
  = Not 
  | Neg 
  | Pos
  deriving (Show)

data Bop 
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | IsEq
  | And
  | Or
  deriving (Show)

data Statement
  = Assign   Variable [Flag] Expression
  | If       Expression Statement Statement
  | While    Expression Statement
  | Sequence Statement  Statement
  | Skip
  | Print    Expression 
  | Function [Variable] Statement
  | Return   Expression
  | Block    Statement
  deriving (Show)

-- for error messages
data Message 
  = SysUnExpect String
  | UnExpect String
  | Expect String
  | Message String

----------------------------------------------------------------------------------------------
-- | `WState` is the "State" maintained by the interpreter's State-Transformer Monad
--   if ws :: WState, you can "access" the `Store` and `Log` as (wStore ws) and (wLog ws)
----------------------------------------------------------------------------------------------

data WState = WS 
  { wStore :: Store -- ^ store mapping Variables to Values 
  , wLog   :: Log   -- ^ list of strings printed during execution  
  } 

-- | A `Log` is the list of messages printed out during execution
type Log      = [String]

-- | `initStore` is the empty state (all variables undefined), log is empty
initStore :: Store  
initStore = Map.empty
