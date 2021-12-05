import CSE230.While.Eval
import System.Environment (getArgs)

main :: IO ()
main = do 
  fs <- getArgs
  mapM_ runFile fs


