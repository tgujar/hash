module Lang where
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec

hashLangDef  :: LanguageDef st
hashLangDef   = emptyDef
                { commentStart   = "/*"
                , commentEnd     = "*/"
                , commentLine    = "//"
                , nestedComments = True
                , identStart     = letter
                , identLetter    = alphaNum <|> oneOf "_'"
                , opStart        = opLetter hashLangDef
                , opLetter       = oneOf ":!#%&*+-\\.<=>^"
                , reservedNames  = ["True", "False", "function", "set", "while", "if", "else", "echo", "return"]
                , reservedOpNames= ["not", "and", "or"]
                , caseSensitive  = True
                }