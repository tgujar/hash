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
scope :: Parser H.Flag
scope = H.Scope <$> oneOf "lgU"

operation :: Parser H.Flag
operation = H.Operation <$> oneOf "qes"

flagParser :: ParsecT String () Data.Functor.Identity.Identity [Flag]
flagParser = do
    _ <- char '-'
    many1 (try operation <|> try scope)

-- Statement Parser
stmtParser :: Parser H.Statement
stmtParser = try funcP 
    <|> try blockP 
    <|> try sequenceP 
    <|> try assignP 
    <|> try ifelseP 
    <|> try whileP 
    <|> try skipP 
    <|> try echoP 
    <|> try returnP
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

funcP :: Parser H.Statement
funcP = do
    _ <- P.reservedOp lexer "function"
    vars <- P.parens lexer $ P.commaSep lexer (P.identifier lexer)
    H.Function vars <$> stmtParser

-- >>> parseFromString stmtParser "ls -la ~/tgujar"
-- Right (External "ls" ["-la","~/tgujar"])
--


-- >>> parseFromString stmtParser "function (x,y,z) { set -l $z $x+$y ; return $z}"
-- Right (Function ["x","y","z"] (Block (Sequence (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))) (Return (Var "z")))))
--

-- >>> parseFromString stmtParser "set -l $z $x+$y ; set -l $z $x+$y"
-- Right (Sequence (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))) (Assign "z" [Scope 'l'] (Op Plus (Var "x") (Var "y"))))
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
        Nothing -> return $ H.Assign v [] e
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


returnP :: Parser H.Statement
returnP = do
    _ <- P.symbol lexer "return"
    H.Return <$> exprParser

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


{--
The syntax uses ";" for new lines and avoids using newline characters as seperator between statements.
It so happens that lexeme consumes newlines as well and I was too deep to go back and change evrything
so that the lang could support newlines as well.
--}
sequenceP :: Parser H.Statement
sequenceP = do
    s1 <- try funcP <|> try assignP <|> try ifelseP <|> try whileP <|> try skipP <|> try echoP <|> try returnP <|> externP
    P.lexeme lexer (char ';')
    s2 <- stmtParser
    optional $ P.lexeme lexer (char ';')
    return $ H.Sequence s1 s2



parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p = runParser p () "Error"


--- >>> parseFromString exprParser "2+(3*4-3)/2"
--- Right (Op Plus (Val (NumVal (Left 2))) (Op Divide (Op Minus (Op Times (Val (NumVal (Left 3))) (Val (NumVal (Left 4)))) (Val (NumVal (Left 3)))) (Val (NumVal (Left 2)))))
---

-- >>> parseFromString exprParser "not $cat"
-- Right (PrefixOp Not (Var "cat"))
--



-----------------------------------test for prefix operator -------------------------------------------------
-- >>> parseFromString exprParser "1.2 + 3.4"
-- Right (Op Plus (Val (NumVal (Right 1.2))) (Val (NumVal (Right 3.4))))
--

-- >>> parseFromString exprParser "-1+4"
-- Right (Op Plus (PrefixOp Neg (Val (NumVal (Left 1)))) (Val (NumVal (Left 4))))
--

-- >>> parseFromString exprParser "\"cat\" + \"dog\""
-- Right (Op Plus (Val (StrVal "cat")) (Val (StrVal "dog")))
--

-- >>> parseFromString exprParser "1+2*3 >= 4"
-- Right (Op Ge (Op Plus (Val (NumVal (Left 1))) (Op Times (Val (NumVal (Left 2))) (Val (NumVal (Left 3))))) (Val (NumVal (Left 4))))
--


-------------------------------------------------------------------------------------------------------------


-----------------------------------test for string parser-----------------------------------------------------
-- >>> parseFromString exprParser "\'hello\'"
-- Left "DUMMY" (line 1, column 1):
-- unexpected "'"
-- expecting number, identifier, literal string or "("
--

-- >>> parseFromString stringParser "(123)=_*&^%$#@!'"
-- Right (Val (StrVal "(123)=_*&^%$#@!"))
--

