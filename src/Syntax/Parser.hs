module Syntax.Parser (parseLambdaExpr) where

import Syntax.Ast
import Data.Void
import Text.Megaparsec (Parsec, between, many, (<|>), choice, MonadParsec (try, eof), runParser, errorBundlePretty)
import Data.Text (Text, pack)
import Text.Megaparsec.Char (space1, letterChar, alphaNumChar, char)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- a variable name must start with a letter, and can be followed by any sequence of letters and numbers
ident :: Parser String
ident = lexeme $ (:) <$> letterChar <*> many alphaNumChar

var :: Parser (Var Plain)
var = Var () <$> ident

variable :: Parser PlainExpr
variable = VarE <$> var

lambda :: Parser PlainExpr
lambda = do
    _ <- lexeme (char '\\' <|> char 'λ')
    v <- var
    _ <- symbol "."
    Lam () v <$> expr

expr :: Parser PlainExpr
expr = choice
    [ try lambda
    , try variable
    , parens application
    ]

application :: Parser PlainExpr
application = do
    hd <- expr
    args <- many expr
    pure $ foldl (App ()) hd args

parseLambdaExpr
  :: FilePath
  -> Text
  -> Either (Text, String) PlainExpr
parseLambdaExpr srcName input =
  case runParser (between sc eof expr) srcName input of
    Left err  -> Left (pack srcName, errorBundlePretty err)
    Right ast -> Right ast
