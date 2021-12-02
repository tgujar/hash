module Parse where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)


import Types as H
import qualified Text.Parsec.Expr as E


lexer       = P.makeTokenParser emptyDef 
-- parens      = P.parens lexer
braces      = P.braces lexer
naturalOrFloat = P.naturalOrFloat lexer
parens = P.parens lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer


exprParser :: Parser H.Expression 
exprParser = E.buildExpressionParser table term

term = num <|> str <|> parens exprParser
num = H.Val . H.NumVal <$> naturalOrFloat
str = H.Var <$> stringLiteral


-- TODO: add prefix operation
-- [prefix "-", prefix "+"]
-- [prefix "not"]

-- [binary "=" E.AssocRight]
-- [binary "and" E.AssocLeft]
-- [binary "or" E.AssocLeft]

table = [[binary "^" E.AssocLeft]
        ,[binary "*" E.AssocLeft
         ,binary "/" E.AssocLeft
         ,binary "%" E.AssocLeft]
        ,[binary "+" E.AssocLeft
         ,binary "-" E.AssocLeft]
        ,[binary "<" E.AssocNone
         ,binary ">" E.AssocNone]
        ]
  where
    binary name assoc =
        E.Infix (mkBinOp name <$ symbol name) assoc
    mkBinOp nm a b = H.Op (binOpMapping nm) a b

binOpMapping :: [Char] -> Bop
binOpMapping "+" = (Plus)
binOpMapping "/" = (Divide)
binOpMapping "*" = (Times)
binOpMapping "-" = (Minus)
binOpMapping "<" = (Lt)
binOpMapping ">" = (Gt)
-- binOpMapping "^" = (^)



parseFromString :: Parser a -> String -> Either ParseError a 
parseFromString p s = runParser p () "DUMMY" s 

--- >>> parseFromString exprParser "2+(3*4-3)/2"
--- Right (Op Plus (Val (NumVal (Left 2))) (Op Divide (Op Minus (Op Times (Val (NumVal (Left 3))) (Val (NumVal (Left 4)))) (Val (NumVal (Left 3)))) (Val (NumVal (Left 2)))))
---


-- >>> parseFromString exprParser "1 + 2 + 3"
-- Right (Op Plus (Op Plus (Val (NumVal (Left 1))) (Val (NumVal (Left 2)))) (Val (NumVal (Left 3))))
--

-- -------------------------------------------------------------------------------
-- -- | Parsing Constants
-- -------------------------------------------------------------------------------

-- -- First, we will write parsers for the `Value` type

-- valueP :: Parser H.Value
-- valueP = intP <|> boolP

-- -- First, fill in the implementation of `intP`. You can assume that the numbers
-- -- in our language are non-negative.

-- intP :: Parser H.Value
-- intP = do 
--    n <- many1 digit
--    return (H.IntVal (read n))

-- floatP :: Parser H.Value
-- floatP = do
--    n <- many1 digit
--    char '.'
--    d <- many1 digit
--    return (H.FloatVal (read $ n ++ "." ++ d))
-- -- Next, define a parser that will accept a particular string `s` as a given value `x`

-- constP :: String -> a -> Parser a
-- constP s x = do 
--    string s
--    return x

-- -- Use the above to define a parser for boolean values 
-- -- where `"true"` and `"false"` should be parsed appropriately.

-- boolP :: Parser H.Value
-- boolP = constP "true"  (H.BoolVal True) <|> constP "false" (H.BoolVal False)
-- -- Continue to use the above to parse the binary operators

-- opP :: Parser H.Bop
-- opP = try (constP "*"  H.Times)
--   <|> try (constP "/"  H.Divide)
--   <|> try (constP "+"  H.Plus )
--   <|> try (constP "-"  H.Minus)
--   <|> try (constP ">=" H.Ge) 
--   <|> try (constP "<=" H.Le)
--   <|> try (constP ">"  H.Gt)
--   <|> try (constP "<"  H.Lt)
--   <|> try (constP "==" H.IsEq)

-- -------------------------------------------------------------------------------
-- -- | Parsing Expressions 
-- -------------------------------------------------------------------------------

-- -- The following is a parser for variables, which are one-or-more uppercase letters. 

-- varP :: Parser H.Variable
-- varP = many1 upper

-- -- Use the above to write a parser for `Expression` values.
-- -- Incomplete, operator precedence is not correct yet.
-- exprP :: Parser H.Expression
-- exprP   =  try multiDivP <|> try othersP <|> try parensP <|> try oneVar <|> try oneVal 
--       where
--         oneVar = do
--           v <- varP; return (H.Var v)
--         oneVal = do
--           v <- valueP; return (H.Val v)
--         parensP = do
--           string "("
--           x <- exprP
--           string ")"
--           return (x)
--         -- multiply and divide have higher precedence than add and subtract
--         multiDivP = do
--           x <- try parensP <|> try oneVar <|> try oneVal
--           spaces
--           op <- try (constP "*" H.Times) <|> try (constP "/" H.Divide)
--           spaces
--           y <- exprP
--           return (H.Op op x y)
--         -- lower precedence than multiply and divide
--         othersP = do
--           x <- try parensP <|> try oneVar <|> try oneVal
--           spaces
--           op <- opP
--           spaces
--           y <- exprP
--           return (H.Op op x y)
--         -- exprP2 = do
--         --   x <- try parensP <|> try oneVar <|> try oneVal
--         --   spaces
--         --   o <- opP
--         --   spaces
--         --   y <- exprP
--         --   return (H.Op o x y)

-- --- >>> parseFromString exprP "1+2*3"
-- --- Right (Op Plus (Val (IntVal 1)) (Op Times (Val (IntVal 2)) (Val (IntVal 3))))
-- ---




-- -------------------------------------------------------------------------------
-- -- | Parsing Statements 
-- -------------------------------------------------------------------------------

-- -- Next, use the expression parsers to build a statement parser

-- statementP :: Parser H.Statement
-- statementP = try sequenceP <|> try assignP <|> try ifP <|> try whileP <|> try skipP

-- assignP :: Parser H.Statement        
-- assignP = do
--   v <- varP
--   spaces
--   string ":="
--   spaces
--   e <- exprP
--   spaces
--   return (H.Assign v e)

-- -- if CONDITION; COMMANDS_TRUE...;
-- -- [else if CONDITION2; COMMANDS_TRUE2...;]
-- -- [else; COMMANDS_FALSE...;]
-- -- end
-- -- https://fishshell.com/docs/current/cmds/if.html
-- ifP :: Parser H.Statement
-- ifP = do
--   string "if"
--   spaces
--   e  <- exprP
--   spaces
--   s1 <- statementP
--   spaces
--   string "else"
--   spaces
--   s2 <- statementP
--   spaces
--   string "end"
--   return (H.If e s1 s2)

-- -- while CONDITION; COMMANDS...; end
-- -- https://fishshell.com/docs/current/cmds/while.html
-- whileP :: Parser H.Statement
-- whileP = do
--   string "while"
--   spaces
--   e  <- exprP
--   string ";"
--   spaces
--   s  <- sequenceP
--   string ";"
--   spaces
--   string "end"
--   return (H.While e s)

-- -- echo [OPTIONS] [STRING]
-- -- we will ignore the options for now
-- -- https://fishshell.com/docs/current/cmds/echo.html
-- echoP :: Parser H.Statement
-- echoP = do
--   string "echo"
--   spaces
--   e  <- exprP
--   spaces
--   return (H.Print e)

-- sequenceP :: Parser H.Statement
-- sequenceP = do
--   s1 <- try assignP <|> try ifP <|>try whileP <|> try skipP
--   spaces
--   string ";"
--   spaces
--   s2 <- statementP
--   return (H.Sequence s1 s2)

-- skipP :: Parser H.Statement   
-- skipP = constP "skip" H.Skip        
        
        

-- -- When you are done, we can put the parser and evaluator together 
-- -- in the end-to-end interpreter function `runFile` in `Main.hs`

-- -- | Parsing Files 

-- -------------------------------------------------------------------------------
-- parseFile :: FilePath -> IO (Either ParseError H.Statement)
-- -------------------------------------------------------------------------------
-- -- >>> ((Right H.w_fact) ==) <$> parseFile "test/in/fact.imp"
-- -- True
-- --
-- -- >>> ((Right H.w_test) == ) <$> parseFile "test/in/test.imp"
-- -- True
-- --
-- -- >>> ((Right H.w_abs) == ) <$> parseFile "test/in/abs.imp"
-- -- True
-- --
-- -- >>> ((Right H.w_times) == ) <$> parseFile "test/in/times.imp"
-- -- True
-- --


-- parseFile f = parseFromFile statementP f