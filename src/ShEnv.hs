module ShEnv where

import System.Process
import System.IO
import GHC.IO.Exception

type Cwd = Maybe FilePath
type Env = Maybe [(String, String)]

