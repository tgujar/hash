module CSE230.While.Parse where

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import qualified CSE230.While.Types as H

{- As you can see, it is rather tedious to write the above tests! 
   They correspond to the code in the files `test.imp` and `fact.imp`. 
   It is rather tedious to have to specify individual programs as Haskell
   values. For this problem, you will use parser combinators to build a parser
   for the WHILE language from the previous problem.
-}

parseFromString :: Parser a -> String -> Either ParseError a 
parseFromString p s = runParser p () "DUMMY" s 

-- >>> parseFromString varP "X45"
-- Right "X"
--

-------------------------------------------------------------------------------
-- | Parsing Constants
-------------------------------------------------------------------------------

-- First, we will write parsers for the `Value` type

valueP :: Parser H.Value
valueP = intP <|> boolP

-- First, fill in the implementation of `intP`. You can assume that the numbers
-- in our language are non-negative.

intP :: Parser H.Value
intP = do
   i <- many1 digit  -- the digits in the fornt of the string
   return (H.IntVal (read i))  -- transform them to Int

-- Next, define a parser that will accept a particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
   _ <- string s
   return x  -- the left part becomes x instead of s


-- Use the above to define a parser for boolean values 
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser H.Value
boolP = constP "true" (H.BoolVal True)  -- try to pasrse "true" and convert it to a boolean value  
    <|> constP "false" (H.BoolVal False)

-- Continue to use the above to parse the binary operators

opP :: Parser H.Bop  -- check each operator
opP    = constP "+"  H.Plus
   <|>   constP "-"  H.Minus
   <|>   constP "*"  H.Times
   <|>   constP "/"  H.Divide
   <|>   constP ">"  H.Gt
   <|>   constP ">=" H.Ge
   <|>   constP "<"  H.Lt
   <|>   constP "<=" H.Le

-------------------------------------------------------------------------------
-- | Parsing Expressions 
-------------------------------------------------------------------------------

-- The following is a parser for variables, which are one-or-more uppercase letters. 

varP :: Parser H.Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values. Assume that
-- operators are right associative, and they all have the same precedence.

between :: String -> String -> Parser p -> Parser p
between s1 s2 p = do
   string s1  -- parse the first string
   m <- p  -- use the parser
   string s2  -- parse the last string
   return m  -- only the middle string is important

parens :: Parser p -> Parser p
parens = between "(" ")"  -- parse the string between the paranthesis

whitespace :: Parser ()
whitespace = skipMany space

exprVal :: Parser H.Expression
exprVal = do
   x <- valueP  -- parse the value
   return (H.Val x)

exprVar :: Parser H.Expression
exprVar = do
   x <- varP  -- parse the variable
   return (H.Var x)

exprOp :: Parser H.Expression
exprOp = do
   x <- exprVar <|> exprVal <|> (parens exprOp)  -- parse a value or variable, or remove "()" first
   whitespace
   o <- opP
   whitespace
   y <- try(exprOp) <|> exprVar <|> exprVal <|> (parens exprOp)
   return (H.Op o x y)

exprP :: Parser H.Expression
exprP   = do
   choice[try(exprOp), exprVal, exprVar]  -- use try() to pretend the input is not consumed


-------------------------------------------------------------------------------
-- | Parsing Statements 
-------------------------------------------------------------------------------

-- Next, use the expression parsers to build a statement parser

assignP :: Parser H.Statement
assignP = do
   x <- varP  -- parse the variable
   string " := "  -- parse the assignment operator
   e <- exprP  -- parse the expression
   return (H.Assign x e)

ifP :: Parser H.Statement
ifP = do
   string "if"
   whitespace
   e <- exprP
   whitespace
   string "then"
   whitespace
   s1 <- statementP
   whitespace 
   string "else" 
   whitespace
   s2 <- statementP
   whitespace
   string "endif" 
   return (H.If e s1 s2)

whileP :: Parser H.Statement
whileP = do
   string "while" 
   whitespace
   e <- exprP
   whitespace
   string "do"
   whitespace
   s <- statementP
   whitespace
   string "endwhile"
   return (H.While e s)

skipP :: Parser H.Statement
skipP = do
   string "skip"
   return (H.Skip)

seqP :: Parser H.Statement
seqP = do
   s1 <- assignP <|> ifP <|> whileP <|> skipP
   string ";"
   whitespace
   s2 <- statementP
   return (H.Sequence s1 s2)

statementP :: Parser H.Statement
statementP = choice [try(seqP), assignP, ifP, whileP, skipP]

{-
ZACK:

cdP :: Parser H.Statement
cdP = do
   string "cd"
   whitespace
   e <- exprP

pwdP :: Parser H.Statement
pwdP = do
   string "pwd"

lsP :: Parser H.Statement
lsP = do
   string "ls"

echoP :: Parser H.Statement
echoP = do
   String "echo"
   whitespace
   e <- exprP
-}


-- When you are done, we can put the parser and evaluator together 
-- in the end-to-end interpreter function `runFile` in `Main.hs`

-- | Parsing Files 

-------------------------------------------------------------------------------
parseFile :: FilePath -> IO (Either ParseError H.Statement)
-------------------------------------------------------------------------------
-- >>> ((Right H.w_fact) ==) <$> parseFile "test/in/fact.imp"
-- True
--
-- >>> ((Right H.w_test) == ) <$> parseFile "test/in/test.imp"
-- True
--
-- >>> ((Right H.w_abs) == ) <$> parseFile "test/in/abs.imp"
-- True
--
-- >>> ((Right H.w_times) == ) <$> parseFile "test/in/times.imp"
-- True
--


parseFile f = parseFromFile statementP f
