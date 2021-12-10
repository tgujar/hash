import Eval (runCmdCheckStore)
import Types
import qualified Data.Map as M

main :: IO ()
main = testParseEval

parseEvalTestCases :: [(String, WState, WState)]
parseEvalTestCases = [
        ("set cat 5", WS initStore [] "", WS [M.fromList [("cat", NumVal (Left 5))]] [] ""),
        ("set dog 3; echo $dog + 5", WS initStore [] "", WS [M.fromList [("dog", NumVal (Left 3))]] ["8"] ""),
        ("set dog 3; echo $dog + 5", WS initStore [] "/someDir", WS [M.fromList [("dog", NumVal (Left 3))]] ["8"] "/someDir"),
        ("echo 10 - 5 + 2", WS initStore [] "", WS [M.empty] ["7"] ""),
        ("if 5 < 3 { set -g c 2 } else { set -g c 3.3 }", WS initStore [] "", WS [M.fromList [("c", NumVal (Right 3.3))]] [] "")
    ]

testParseEval :: IO ()
testParseEval = do
        results <- mapM (\(cmd, initialState, expectedFinal) -> runCmdCheckStore cmd initialState expectedFinal) parseEvalTestCases
        if and results then print "All parseEval tests passed" else print "Some parseEval tests failed"
