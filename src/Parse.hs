module Parse where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Types as H
import qualified Text.Parsec.Expr as E
import Lang as L
import qualified Control.Monad.Identity as Data.Functor.Identity
import Text.Parsec.Token (GenTokenParser(identifier))
import qualified Test.QuickCheck.Function as H
import Text.Parsec.Error (messageString, Message(..), errorMessages)

lexer       = P.makeTokenParser L.hashLangDef


{--
For some reason adding "$" to the list of reserved operators and creating an entry in the table
to parse variables as "$var" doesn't work well with you have an expression like
"$x+$y". I have no idea why and so I have parsed it seperately and removed it from the list of reserved Ops.
--}
table :: [[E.Operator String u Data.Functor.Identity.Identity Expression]]
table = [[prefix "-" (H.PrefixOp H.Neg), prefix "+" (H.PrefixOp H.Pos)]
        ,[prefix "not" (H.PrefixOp Not)]
        ,[binary "*" (H.Op Times) E.AssocLeft, binary "/" (H.Op Divide) E.AssocLeft, binary "%" (H.Op Divide) E.AssocLeft]
        ,[binary "+" (H.Op Plus) E.AssocLeft, binary "-" (H.Op Minus) E.AssocLeft]
        ,[binary ">=" (H.Op Ge) E.AssocNone ,binary "<=" (H.Op Le) E.AssocNone]
        ,[binary ">" (H.Op Gt) E.AssocNone ,binary "<" (H.Op Lt) E.AssocNone]
        ,[binary "and" (H.Op And) E.AssocLeft]
        ,[binary "or" (H.Op Or) E.AssocLeft]
        ]
  where
    binary  name fun assoc = E.Infix (do{ P.reservedOp lexer name; return fun }) assoc
    prefix  name fun       = E.Prefix (do{ P.reservedOp lexer name; return fun })

-- Expression Parser 
-- Using the module Text.Parsec.Expr to build an expression parser
exprParser :: Parser H.Expression
exprParser = E.buildExpressionParser table expr

expr :: Parser H.Expression
expr = var <|> num <|> str <|> bool <|> P.parens lexer exprParser

-- Expression type parsers

-- Parser ints or floats
num :: Parser H.Expression
num = H.Val . H.NumVal <$> P.naturalOrFloat lexer

-- Parser for variable access
-- -- refer https://fishshell.com/docs/current/language.html
var :: Parser H.Expression
var = H.Var <$> do {_ <- string "$"; P.identifier lexer}

-- Parser for string literals
str :: Parser H.Expression
str =  H.Val . H.StrVal <$> P.lexeme lexer (P.stringLiteral lexer)

-- Parser for bollean values
bool :: Parser H.Expression
bool = H.Val . H.BoolVal <$> do {
    str <- try $ P.symbol lexer "True" <|> P.symbol lexer "False";
    case str of
        "True" -> return True
        _ -> return False
}

-- Flag Parser for "set" command
-- for set of flags refer https://fishshell.com/docs/current/cmds/set.html?highlight=set
scope :: Parser H.RefScope 
scope = do
    f <- oneOf "lgU"
    case f of
        'l' -> return Local
        'g' -> return Global 
        _ -> return Universal 


flagParser :: ParsecT String () Data.Functor.Identity.Identity RefScope 
flagParser = do
    _ <- char '-'
    scope

-- Statement Parser
stmtParser :: Parser H.Statement
stmtParser = try blockP 
    <|> try sequenceP 
    <|> try assignP 
    <|> try ifelseP 
    <|> try whileP 
    <|> try skipP 
    <|> try echoP 
    <|> try hashFileP
    <|> externP

-- Statement Type parsers
-- Functions have the following syntax
{--
funtion (x, y, z, ...) {
    statement1
    statement2; statement 3
}
--}

externP :: Parser H.Statement
externP = do
    ext <- sepBy1 (many1 (alphaNum <|> oneOf "~/-.")) spaces
    return $ H.External (head ext) (tail ext)

-- Parser for function syntax
-- funcP :: Parser H.Statement
-- funcP = do
--     _ <- P.reservedOp lexer "function"
--     vars <- P.parens lexer $ P.commaSep lexer (P.identifier lexer)
--     H.Function vars <$> stmtParser

-- >>> parseFromString stmtParser "ls -la ~/tgujar"
-- Right (External "ls" ["-la","~/tgujar"])
--


-- >>> parseFromString stmtParser "function (x,y,z) { set -l $z $x+$y ; return $z}"
-- Right (Function ["x","y","z"] (Block (Sequence (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))) (Return (Var "z")))))
--

-- >>> parseFromString stmtParser "set -l $z $x+$y ; set -l $z $x+$y"
-- Right (Sequence (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))) (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))))
--

-- >>> parseFromString stmtParser "hash test.hash"
-- Right (HashFile "test.hash")
--

-- syntax for assignments is same as https://fishshell.com/docs/current/cmds/set.html?highlight=set
assignP :: Parser H.Statement
assignP = do
    _ <- P.symbol lexer "set"
    flags <- optionMaybe flagParser
    spaces
    v <- P.identifier lexer
    e <- exprParser
    case flags of
        Nothing -> return $ H.Assign v Local e
        (Just f) -> return $ H.Assign v f e

blockP :: Parser H.Statement
blockP = H.Block <$> P.braces lexer stmtParser


-- There's no nested if syntax presently
-- if .. else syntax is as follows
{--
if expr {
    statement1;
    statement2
    ...   
}
else {
    statement1;
    statement2;
    ...
}
--}
ifelseP :: Parser H.Statement
ifelseP = do
    _ <- P.lexeme lexer $ string "if"
    test <- exprParser
    trueSt <- P.braces lexer stmtParser
    _ <- P.lexeme lexer $ string "else"
    falseSt <- P.braces lexer stmtParser
    return $ H.If test trueSt falseSt

-- Parser for the return statement of the funtion
-- returnP :: Parser H.Statement
-- returnP = do
--     _ <- P.symbol lexer "return"
--     H.Return <$> exprParser

whileP :: Parser H.Statement
whileP = do
    _ <- P.lexeme lexer $ string "while"
    test <- exprParser
    stmts <- P.braces lexer stmtParser
    return $ H.While test stmts

skipP :: Parser H.Statement
skipP = do
    _ <- P.lexeme lexer $ string "skip"
    return H.Skip

echoP :: Parser H.Statement
echoP = do
    _ <- P.lexeme lexer $ string "echo"
    H.Print <$> exprParser
    

-- Reference for Text.Parsec.Error: https://hackage.haskell.org/package/parsec-3.1.15.0/docs/Text-Parsec-Error.html
errorP :: Parser H.Statement
-- errorP e = "Parse error at " ++ show (errorPos e) ++ ": " ++ show (errorMessages e)
-- errorP e = error $ "Parse error at " ++ show e
errorP = do
    _ <- P.lexeme lexer $ string "error"
    return $ H.Error

{--
The syntax uses ";" for new lines and avoids using newline characters as seperator between statements.
It so happens that lexeme consumes newlines as well and I was too deep to go back and change evrything
so that the lang could support newlines as well.
--}
sequenceP :: Parser H.Statement
sequenceP = do
    s1 <- try assignP <|> try ifelseP <|> try whileP <|> try skipP <|> try echoP <|> try hashFileP <|> externP
    P.lexeme lexer (char ';')
    s2 <- stmtParser
    optional $ P.lexeme lexer (char ';')
    return $ H.Sequence s1 s2

hashFileP :: Parser H.Statement
hashFileP = do
    _ <- P.lexeme lexer (string "hash")
    H.HashFile <$> P.stringLiteral lexer


parseFromStringIO :: Parser a -> String -> IO (Either ParseError a)
parseFromStringIO p s = return $ runParser p () "Parse Error" s

parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p = runParser p () "Parse Error"

-----------------------------------test for string parser-----------------------------------------------------
-- >>> parseFromString exprParser "cat"
-- Right (Var "cat")
--

-- >>> parseFromString exprParser "\"cat\""
-- Right (Val "cat")
--

-- >>> parseFromString exprParser "(123)=_*&^%$#@!'"
-- Right (Val 123)
--

-- >>> parseFromString exprParser "2+(3*4-3)/2"
-- Right (Op Plus (Val 2) (Op Divide (Op Minus (Op Times (Val 3) (Val 4)) (Val 3)) (Val 2)))
--

-- >>> parseFromString exprParser "28%5"
-- Right (Op Divide (Val 28) (Val 5))
--

-- >>> parseFromString exprParser "-5>=2"
-- Right (Op Ge (PrefixOp Neg (Val 5)) (Val 2))
--

-- >>> parseFromString exprParser "not $cat"
-- Right (PrefixOp Not (Var "cat"))
--

-- >>> parseFromString exprParser "not $cat and $dog"
-- Right (Op And (PrefixOp Not (Var "cat")) (Var "dog"))
--

-- >>> parseFromString exprParser "$cat or $dog"
-- Right (Op Or (Var "cat") (Var "dog"))
--

-----------------------------------test for operator -------------------------------------------------
-- >>> parseFromString exprParser "1.2 + 3.4"
-- Right (Op Plus (Val 1.2) (Val 3.4))
--

-- >>> parseFromString exprParser "-1+4"
-- Right (Op Plus (PrefixOp Neg (Val 1)) (Val 4))
--

-- >>> parseFromString exprParser "\"cat\" + \"dog\""
-- Right (Op Plus (Val "cat") (Val "dog"))
--

-- >>> parseFromString exprParser "$cat + $dog"
-- Right (Op Plus (Var "cat") (Var "dog"))
--


-- >>> parseFromString exprParser "1+2*3 >= 4"
-- Right (Op Ge (Op Plus (Val 1) (Op Times (Val 2) (Val 3))) (Val 4))
--


-----------------------------------test for ifelse -------------------------------------------------
-- >>> parseFromString stmtParser "if (1+2) { echo 1; echo 2; } else { echo 3; echo 4; }"
-- Right (If (Op Plus (Val 1) (Val 2)) (Sequence (Print (Val 1)) (Print (Val 2))) (Sequence (Print (Val 3)) (Print (Val 4))))
--

-----------------------------------test for while -------------------------------------------------
-- >>> parseFromString stmtParser "while (1+2) { echo 1; echo 2; }"
-- Right (While (Op Plus (Val 1) (Val 2)) (Sequence (Print (Val 1)) (Print (Val 2))))
--

-----------------------------------test for return -------------------------------------------------
-- >>> parseFromString stmtParser "return 1+2"
-- Right (Return (Op Plus (Val 1) (Val 2)))
--

-- >>> parseFromString stmtParser "return $cat"
-- Right (Return (Var "cat"))
--

-- >>> parseFromString stmtParser "return cat + $dog"
-- Right (Return (Op Plus (Var "cat") (Var "dog")))
--


-----------------------------------test for echo -------------------------------------------------
-- >>> parseFromString stmtParser "echo \"hello\""
-- Right (Print (Val "hello"))
--

-- >>> parseFromString stmtParser "echo \"hello\"; echo \"world\""
-- Right (Sequence (Print (Val "hello")) (Print (Val "world")))
--

-- >>> parseFromString stmtParser "echo $cat"
-- Right (Print (Var "cat"))
--

-----------------------------------test for skip -------------------------------------------------
-- >>> parseFromString stmtParser "skip"
-- Right Skip
--

-----------------------------------test for errors------------------------------------------------------------
-- >>> parseFromString exprParser "cat"
-- Left "Error" (line 1, column 1):
-- unexpected "c"
-- expecting "$", number, literal string, "True", "False" or "("
--

-- >>> parseFromString exprParser "$"
-- Left "Error" (line 1, column 2):
-- unexpected end of input
-- expecting identifier
--

-- >>> parseFromString exprParser "1+"
-- Left "Error" (line 1, column 3):
-- unexpected end of input
-- expecting end of "+", "$", number, literal string, "True", "False" or "("
--

-- >>> parseFromString exprParser "not"
-- Left "Error" (line 1, column 4):
-- unexpected end of input
-- expecting end of "not", "$", number, literal string, "True", "False" or "("
--

-- >>> parseFromString stmtParser "return cat + dog"
-- Left "Error" (line 1, column 12):
-- unexpected "+"
-- expecting space or letter or digit
--