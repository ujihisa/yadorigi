
module Yadorigi.Parser.Tokenizer (tokenizer) where

import Yadorigi.Parser.DataTypes

import Text.Parsec

import Data.Char
import Data.Maybe

import Control.Monad
import Control.Applicative ((<$>))

-- Constant Values

reservedStr :: [String]
reservedStr = reservedWord++reservedSymbol

reservedWord :: [String]
reservedWord = ["if","then","else","case","of","let","in"]

reservedSymbol :: [String]
reservedSymbol = ["=","@","\\","->","=>","::","|",",","(",")","[","]"]

-- Tokenizer

getToken :: Parsec String u Token'
getToken =
    do pos <- getPosition
       body <- literalTokenizer <|> nameOpTokenizer <|> reservedTokenizer <?> "valid token"
       spacesAndComments
       return $ Token' pos body

tokenizer :: Parsec String u TokenStream
tokenizer = between spacesAndComments eof (many getToken)

-- Spaces and Comments

lineComment :: Parsec String u ()
lineComment = try $ string "--" >> manyTill anyChar (forever (char '\n') <|> eof) >> return ()

blockComment :: Parsec String u ()
blockComment = try $ string "{-" >> manyTill anyChar (try (string "-}")) >> return ()

spacesAndComments :: Parsec String u ()
spacesAndComments = (skipMany $ (const () <$> space) <|> lineComment <|> blockComment) <?> "comment"

-- Literal

literalTokenizer :: Parsec String u Token
literalTokenizer = LiteralToken <$> (numLiteralTokenizer <|> stringTokenizer <|> charTokenizer)

numLiteralTokenizer :: Parsec String u Literal
numLiteralTokenizer = (char '0' >> (octTokenizer <|> hexTokenizer)) <|> decTokenizer

decTokenizer :: Parsec String u Literal
decTokenizer =
    do integer <- many1 digit
       fractional <- option "" $ liftM2 (:) (char '.') (many1 digit)
       return $ if null fractional
           then LiteralInt $ read integer
           else LiteralFloat $ read (integer++fractional)

octTokenizer :: Parsec String u Literal
octTokenizer = (char 'o' <|> char 'O') >>
    (LiteralInt . foldl (\i c -> (digitToInt c)+i*8) 0 <$> many1 octDigit)

hexTokenizer :: Parsec String u Literal
hexTokenizer = (char 'x' <|> char 'X') >>
    (LiteralInt . foldl (\i c -> (digitToInt c)+i*16) 0 <$> many1 hexDigit)

strElem :: Parsec String u Char
strElem = noneOf "\\\"\'" <|> liftM2 (\bs -> conv.(`const` bs)) (char '\\') (oneOf "abfnrtv\\\"\'")
    where conv c = fromMaybe c $ lookup c
              [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t'),('v','\v')]

stringTokenizer :: Parsec String u Literal
stringTokenizer = between (string "\"") (string "\"") $ LiteralString <$> many strElem

charTokenizer :: Parsec String u Literal
charTokenizer = between (string "\'") (string "\'") $ LiteralChar <$> strElem

-- Name and Operators

nameOpTokenizer :: Parsec String u Token
nameOpTokenizer =
    do ns <- namespaceTokenizer
       t <- nameTokenizer <|> opTokenizer
       return $ case t of
           (NameToken (ScopedName _ s)) -> if elem s reservedStr && null ns
               then ReservedToken s else NameToken $ ScopedName ns s
           (OpToken (ScopedName _ s)) -> if elem s reservedStr && null ns
               then ReservedToken s else OpToken $ ScopedName ns s

nameTokenizer :: Parsec String u Token
nameTokenizer = (NameToken .ScopedName []) <$>
    liftM2 (:) (letter <|> char '_') (many (alphaNum <|> oneOf "_\'"))

opTokenizer :: Parsec String u Token
opTokenizer = (OpToken .ScopedName []) <$> liftM2 (:) opc (many opc)
    where opc = oneOf "!#$%&*+-./:<=>?@^|"

namespaceTokenizer :: Parsec String u [String]
namespaceTokenizer = many $ try $ liftM3 ((const.).(:))
    upper (many (alphaNum <|> char '_')) (char '.')

-- Reserved Words and Reserved Symbols

reservedTokenizer :: Parsec String u Token
reservedTokenizer = try $ choice $ map ((>>= return.ReservedToken).string) reservedStr

