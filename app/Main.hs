module Main where

import Lib

-- main :: IO ()
-- main = someFunc
import System.Process
import System.IO
import GHC.IO.Handle.Text
import Control.Exception

main :: IO ()
main = do
    res <- try (callProcess "bye" []) :: IO (Either IOException ())
    case res of
        Left ex -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
    print "hello I am running"
    -- do
    -- (Just hin,Just ho1, _, hp1) <- createProcess (shell "python3"){std_out=CreatePipe}
    -- -- hSetBuffering hin NoBuffering
    -- -- hSetBuffering ho1 NoBuffering
    -- sOut <- hGetContents ho1
    -- _ <- waitForProcess hp1
    -- putStrLn sOut