module Utils.Parsers (
    Parser,
    sc,
    lexeme,
    integer,
    symbol,
    comma,
    semicolon,
    colon,
    dot,
    pipe,
    parens,
    skipSpaces,
    manySpaces,
    negativeInteger,
    signedInteger,
    charInRange
) where

import           Control.Monad              (void)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, empty, many,
                                             satisfy)
import           Control.Applicative        ((<|>))
import           Text.Megaparsec.Char       (space1, string, char)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type specialized to operate on 'String' input for this project.
--   This is a thin alias around Megaparsec's 'Parsec' with no custom state.
type Parser = Parsec Void String

-- | Skip whitespace and comments according to the lexer's space consumer.
--   Commonly used as the base space consumer for lexemes.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Apply a parser and consume any trailing whitespace using 'sc'.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse an integer, consuming trailing whitespace.
integer :: Parser Int
integer = lexeme L.decimal

-- | Parse a fixed string symbol and consume trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse a comma token (with trailing whitespace).
comma :: Parser String
comma = symbol ","

-- | Parse a semicolon token (with trailing whitespace).
semicolon :: Parser String
semicolon = symbol ";"

-- | Parse a colon token (with trailing whitespace).
colon :: Parser String
colon = symbol ":"

-- | Parse a dot/token `.` (with trailing whitespace).
dot :: Parser String
dot = symbol "."

-- | Parse a pipe `|` token (with trailing whitespace).
pipe :: Parser String
pipe = symbol "|"

-- | Parse something enclosed in parentheses, consuming surrounding whitespace.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse zero-or-more literal spaces (does not consume other whitespace).
skipSpaces :: Parser ()
skipSpaces = void $ many (string " ")

-- | Parse a single character within the inclusive range [start..end].
charInRange :: Char -> Char -> Parser Char
charInRange start end = satisfy (\x -> x >= start && x <= end)

-- Parse zero-or-more literal spaces (keeps parity with existing callers)
manySpaces :: Parser ()
manySpaces = skipSpaces

-- | Parse a negative integer beginning with a '-' sign.
negativeInteger :: Parser Int
negativeInteger = do
  void $ char '-'
  n <- integer
  return (n * (-1))

-- | Parse a signed integer: either a negative integer or a non-negative one.
signedInteger :: Parser Int
signedInteger = negativeInteger <|> integer
