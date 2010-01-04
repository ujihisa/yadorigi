
module Yadorigi.Parser where

import Yadorigi.Parser.DataTypes
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Data.Maybe

-- Constant Values

reservedWord :: [String]
reservedWord = ["if","then","else","case","of","let","in"]

reservedSymbol :: [String]
reservedSymbol = ["=","@","->"]

-- Position

getPos :: GenParser tok st Position
getPos = liftM (\p -> Position (sourceLine p) (sourceColumn p)) getPosition

getPosWithTest :: LayoutInfo -> CharParser st Position
getPosWithTest layout = getPos >>= testPos layout

-- Layout

arbitraryLayout :: LayoutInfo
arbitraryLayout = LayoutInfo False 0

checkLayout :: LayoutInfo -> Position -> Bool
checkLayout (LayoutInfo False n) (Position line column) = n <= column
checkLayout (LayoutInfo True n) (Position line column) = n == column

arbitraryElemLayout :: LayoutInfo -> LayoutInfo
arbitraryElemLayout (LayoutInfo t n) = LayoutInfo False n

tailElemLayout :: LayoutInfo -> LayoutInfo
tailElemLayout (LayoutInfo True n) = LayoutInfo False (n+1)
tailElemLayout layout = layout

-- Parser Combinators

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p = p >>= const (return x)

testPos :: LayoutInfo -> Position -> CharParser st Position
testPos layout pos
    | checkLayout layout pos = return pos
    | otherwise = pzero

keyword :: String -> CharParser st String
keyword str = try $ liftM2 const (string str) $ notFollowedBy $ alphaNum <|> char '_'

keysymbol :: String -> CharParser st String
keysymbol str = try $ liftM2 const (string str) $ notFollowedBy $ oneOf "!#$%&*+-./:<=>?@^"

layoutMany :: LayoutInfo -> (LayoutInfo -> CharParser st a) -> CharParser st [a]
layoutMany layout parser =
    do (Position _ col) <- getPosWithTest layout
       many $ parser $ LayoutInfo True col

layoutMany1 :: LayoutInfo -> (LayoutInfo -> CharParser st a) -> CharParser st [a]
layoutMany1 layout parser =
    do (Position _ col) <- getPosWithTest layout
       many1 $ parser $ LayoutInfo True col

-- Tokenizer

lexer :: LayoutInfo -> (CharParser st t) -> (CharParser st (PlusPos t))
lexer layout strParser = liftM3 (\ a b c -> PlusPos a b)
    (getPosWithTest layout) strParser spacesAndComments

lineCommentToken :: CharParser st ()
lineCommentToken =
    do try $ string "--"
       manyTill anyChar (returnConst () (char '\n') <|> eof)
       return ()

blockCommentToken :: CharParser st ()
blockCommentToken =
    do try $ string "{-"
       manyTill anyChar (try (string "-}"))
       return ()

spacesAndComments :: CharParser st ()
spacesAndComments =
    skipMany $ liftM (const ()) space <|> lineCommentToken <|> blockCommentToken

labelToken :: (CharParser () Char) -> (CharParser () Char) -> [String]
    -> LayoutInfo -> CharParser () (PlusPos String)
labelToken hparser tparser reservedList layout = lexer layout $ try $
      do str <- liftM2 (:) hparser $ many tparser
         if elem str reservedList
            then unexpected $ show str
            else return str

nameToken :: LayoutInfo -> CharParser () (PlusPos String)
nameToken layout = labelToken (letter <|> char '_') (alphaNum <|> char '_') reservedWord layout

opToken :: LayoutInfo -> CharParser () (PlusPos String)
opToken layout = labelToken (oneOf "!#$%&*+-./:<=>?@^") (oneOf "!#$%&*+-./:<=>?@^")
    reservedSymbol layout

vNameToken :: LayoutInfo -> CharParser () (PlusPos String)
vNameToken layout = labelToken (lower <|> char '_') (alphaNum <|> char '_') reservedWord layout

vOpToken :: LayoutInfo -> CharParser () (PlusPos String)
vOpToken layout = labelToken (oneOf "!#$%&*+-./<=>?@^") (oneOf "!#$%&*+-./:<=>?@^")
    reservedSymbol layout

cNameToken :: LayoutInfo -> CharParser () (PlusPos String)
cNameToken layout = labelToken upper (alphaNum <|> char '_') reservedWord layout

cOpToken :: LayoutInfo -> CharParser () (PlusPos String)
cOpToken layout = labelToken (char ':') (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

eofToken :: LayoutInfo -> CharParser () (PlusPos ())
eofToken layout = lexer layout $ returnConst () eof

-- Literal Token

decToken :: CharParser st Literal
decToken =
    do integer <- many1 digit
       fractional <- option "" $ liftM2 (:) (char '.') (many1 digit)
       return $ if null fractional
           then LiteralInt $ read integer
           else LiteralFloat $ read (integer++fractional)

octToken :: CharParser st Literal
octToken = try $
    do char '0'
       char 'o' <|> char 'O'
       liftM (LiteralInt .foldl (\c -> (c*8+).digitToInt) 0) $ many1 octDigit

hexToken :: CharParser st Literal
hexToken = try $
    do char '0'
       char 'x' <|> char 'X'
       liftM (LiteralInt .foldl (\c -> (c*16+).digitToInt) 0) $ many1 hexDigit

strElem :: CharParser st Char
strElem = noneOf "\\\"\'" <|> liftM2 (\bs -> conv.(`const` bs)) (char '\\') (oneOf "abfnrtv\\\"\'")
    where conv c = fromMaybe c $ lookup c
              [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t'),('v','\v')]

stringToken :: CharParser st Literal
stringToken = between (string "\"") (string "\"") $ liftM LiteralString $ many strElem

charToken :: CharParser st Literal
charToken = between (string "\'") (string "\'") $ liftM LiteralChar strElem

numLiteralToken :: LayoutInfo -> CharParser st (PlusPos Literal)
numLiteralToken layout = liftM3 (\a b c -> PlusPos a b)
    (getPosWithTest layout) (hexToken <|> octToken <|> decToken) spacesAndComments

literalToken :: LayoutInfo -> CharParser st (PlusPos Literal)
literalToken layout = liftM3 (\a b c -> PlusPos a b) (getPosWithTest layout)
    (hexToken <|> octToken <|> decToken <|> stringToken <|> charToken) spacesAndComments

-- Expression Parser

exprParser :: Int -> LayoutInfo -> CharParser () Expr
exprParser 0 layout = opExprParser layout
exprParser 1 layout =
    letParser layout <|> ifParser layout <|> caseParser layout <|> exprParser 2 layout
exprParser 2 layout = let tlayout = tailElemLayout layout in
    liftM2 (foldl apply) (exprParser 3 layout) $ many $ exprParser 3 tlayout
    where
        apply l@(Expr pos _) r = applyFunctionExpr pos l r
exprParser 3 layout =
    nameParser layout <|> literalParser layout <|> bracketParser layout <|> listParser layout

opExprParser :: LayoutInfo -> CharParser () Expr
opExprParser layout = let tlayout = tailElemLayout layout in
    do head@(Expr pos _) <- exprParser 1 layout
       do (PlusPos _ op) <- opToken tlayout
          tail <- opExprParser tlayout
          return $ infixExpr pos op head tail
          <|> return head
    <|> liftM2 (\(PlusPos pos _) -> negativeExpr pos)
            (lexer layout $ keysymbol "-") (opExprParser tlayout)

nameParser :: LayoutInfo -> CharParser () Expr
nameParser layout = liftM (\(PlusPos pos body) -> nameExpr pos body) (nameToken layout)

literalParser :: LayoutInfo -> CharParser () Expr
literalParser layout =
    liftM (\(PlusPos pos literal) -> literalExpr pos literal) (literalToken layout)

bracketParser :: LayoutInfo -> CharParser () Expr
bracketParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "(")
       body <- exprParser 0 tlayout
       lexer tlayout (string ")")
       return $ bracketExpr pos body

listParser :: LayoutInfo -> CharParser () Expr
listParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "[")
       body <- sepBy (exprParser 0 tlayout) (lexer tlayout (string ","))
       lexer tlayout (string "]")
       return $ listExpr pos body

letParser :: LayoutInfo -> CharParser () Expr
letParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "let")
       list <- layoutMany1 tlayout let1Parser
       lexer tlayout (keyword "in")
       expr <- exprParser 0 tlayout
       return $ letExpr pos list expr
    where
        let1Parser layout = let tlayout = tailElemLayout layout in
            do pattern <- patternParser 11 layout
               lexer tlayout (keysymbol "=")
               expr <- exprParser 0 tlayout
               return $ LetOne pattern expr

ifParser :: LayoutInfo -> CharParser () Expr
ifParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "if")
       c <- exprParser 0 tlayout
       lexer tlayout (keyword "then")
       t <- exprParser 0 tlayout
       lexer tlayout (keyword "else")
       f <- exprParser 0 tlayout
       return $ ifExpr pos c t f

caseParser :: LayoutInfo -> CharParser () Expr
caseParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "case")
       expr <- exprParser 0 tlayout
       lexer tlayout (keyword "of")
       list <- layoutMany1 tlayout casePatternParser
       return $ caseExpr pos expr list
    where
        casePatternParser layout = let tlayout = tailElemLayout layout in
            do pattern <- patternParser 11 layout
               do lexer tlayout (keysymbol "->")
                  exprParser 0 tlayout >>= return.CasePattern pattern.Left
                  <|> (layoutMany1 tlayout caseGuardParser >>= return.CasePattern pattern.Right)
        caseGuardParser layout = let tlayout = tailElemLayout layout in
            do lexer layout (string "|")
               cond <- exprParser 0 tlayout
               lexer tlayout (keysymbol "->")
               expr <- exprParser 0 tlayout
               return $ CaseGuard cond expr

-- Pattern Match Parser

patternParser :: Int -> LayoutInfo -> CharParser () PatternMatch
patternParser 0 layout = opPatternParser layout
patternParser 10 layout = dcPatternParser layout <|> patternParser 11 layout
patternParser 11 layout =
    literalPatternParser layout <|> asPatternParser layout <|> bracketPatternParser layout <|>
    listPatternParser layout <|> singleDCPatternParser layout

opPatternParser :: LayoutInfo -> CharParser () PatternMatch
opPatternParser layout = let tlayout = tailElemLayout layout in
    do head@(PatternMatch pos _) <- patternParser 10 layout
       do (PlusPos _ cons) <- cOpToken tlayout <|> lexer tlayout (keysymbol "+")
          tail <- opPatternParser tlayout
          return $ dcOpPattern pos cons head tail
          <|> return head
    <|> do (PlusPos pos _) <- lexer layout (keysymbol "-")
           (PlusPos _ num) <- numLiteralToken tlayout
           return $ case num of
               (LiteralInt n) -> literalPattern pos $ LiteralInt $ -n
               (LiteralFloat n) -> literalPattern pos $ LiteralFloat $ -n

dcPatternParser :: LayoutInfo -> CharParser () PatternMatch
dcPatternParser layout =
    do (PlusPos pos cons) <- cNameToken layout
       body <- many (patternParser 11 (tailElemLayout layout))
       return $ dcPattern pos cons body

literalPatternParser :: LayoutInfo -> CharParser () PatternMatch
literalPatternParser layout =
    literalToken layout >>= \(PlusPos pos literal) -> return $ literalPattern pos literal

asPatternParser :: LayoutInfo -> CharParser () PatternMatch
asPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos var) <- vNameToken layout
       do lexer tlayout (keysymbol "@")
          pattern <- patternParser 10 tlayout
          return $ asPattern pos var pattern
          <|> (return $ bindPattern pos var)

singleDCPatternParser :: LayoutInfo -> CharParser () PatternMatch
singleDCPatternParser layout =
    do (PlusPos pos cons) <- cNameToken layout
       return $ dcPattern pos cons []

bracketPatternParser :: LayoutInfo -> CharParser () PatternMatch
bracketPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "(")
       body <- patternParser 0 tlayout
       lexer tlayout (string ")")
       return $ bracketPattern pos body

listPatternParser :: LayoutInfo -> CharParser () PatternMatch
listPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "[")
       body <- sepBy (patternParser 0 tlayout) (lexer tlayout (string ","))
       lexer tlayout (string "]")
       return $ listPattern pos body

-- Global Parser

globalParser :: CharParser () Expr
globalParser =
    do lexer arbitraryLayout (return "")
       result <- exprParser 0 arbitraryLayout
       eofToken arbitraryLayout
       return result

