
module Yadorigi.Parser.Parser where

import Yadorigi.Common
import Yadorigi.Parser.DataTypes
import Text.Parsec
import Control.Monad
import Data.Char
import Data.Maybe

-- Constant Values

reservedWord :: [String]
reservedWord = ["if","then","else","case","of","let","in"]

reservedSymbol :: [String]
reservedSymbol = ["=","@","->","=>","::","|"]

-- Position

getPos :: Parsec s u Position
getPos = liftM (\p -> Position (sourceLine p) (sourceColumn p)) getPosition

getPosWithTest :: LayoutInfo -> Parsec String u Position
getPosWithTest layout = getPos >>= testPos layout

minusPos :: (PlusPos a) -> a
minusPos (PlusPos _ a) = a

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

testPos :: LayoutInfo -> Position -> Parsec String u Position
testPos layout pos
    | checkLayout layout pos = return pos
    | otherwise = mzero

keyword :: String -> Parsec String u String
keyword str = try $ liftM2 const (string str) $ notFollowedBy $ alphaNum <|> char '_'

keysymbol :: String -> Parsec String u String
keysymbol str = try $ liftM2 const (string str) $ notFollowedBy $ oneOf "!#$%&*+-./:<=>?@^|"

layoutMany :: LayoutInfo -> (LayoutInfo -> Parsec String u a) -> Parsec String u [a]
layoutMany layout parser =
    getPosWithTest layout >>= \(Position _ col) -> many $ parser $ LayoutInfo True col

layoutMany1 :: LayoutInfo -> (LayoutInfo -> Parsec String u a) -> Parsec String u [a]
layoutMany1 layout parser =
    getPosWithTest layout >>= \(Position _ col) -> many1 $ parser $ LayoutInfo True col

-- Tokenizer

lexer :: LayoutInfo -> (Parsec String u t) -> (Parsec String u (PlusPos t))
lexer layout strParser =
    liftM3 (\ a b c -> PlusPos a b) (getPosWithTest layout) strParser spacesAndComments

lineCommentToken :: Parsec String u ()
lineCommentToken =
    try $ string "--" >> manyTill anyChar ((char '\n'>>return ()) <|> eof) >> return ()

blockCommentToken :: Parsec String u ()
blockCommentToken = try $ string "{-" >> manyTill anyChar (try (string "-}")) >> return ()

spacesAndComments :: Parsec String u ()
spacesAndComments = skipMany $ liftM (const ()) space <|> lineCommentToken <|> blockCommentToken

labelToken :: (Parsec String u Char) -> (Parsec String u Char)
    -> [String] -> LayoutInfo -> Parsec String u (PlusPos String)
labelToken hparser tparser reservedList layout = lexer layout $ try $
      do str <- liftM2 (:) hparser $ many tparser
         if elem str reservedList
            then unexpected $ show str
            else return str

nameToken :: LayoutInfo -> Parsec String u (PlusPos String)
nameToken = labelToken (letter <|> char '_') (alphaNum <|> oneOf "_\'") reservedWord

opToken :: LayoutInfo -> Parsec String u (PlusPos String)
opToken = labelToken (oneOf "!#$%&*+-./:<=>?@^|") (oneOf "!#$%&*+-./:<=>?@^|") reservedSymbol

vNameToken :: LayoutInfo -> Parsec String u (PlusPos String)
vNameToken = labelToken (lower <|> char '_') (alphaNum <|> oneOf "_\'") reservedWord

vOpToken :: LayoutInfo -> Parsec String u (PlusPos String)
vOpToken = labelToken (oneOf "!#$%&*+-./<=>?@^|") (oneOf "!#$%&*+-./:<=>?@^|") reservedSymbol

cNameToken :: LayoutInfo -> Parsec String u (PlusPos String)
cNameToken = labelToken upper (alphaNum <|> oneOf "_\'") reservedWord

cOpToken :: LayoutInfo -> Parsec String u (PlusPos String)
cOpToken = labelToken (char ':') (oneOf "!#$%&*+-./:<=>?@^|") reservedSymbol

eofToken :: LayoutInfo -> Parsec String u (PlusPos ())
eofToken layout = lexer layout $ eof>>return ()

-- Literal Token

decToken :: Parsec String u Literal
decToken =
    do integer <- many1 digit
       fractional <- option "" $ liftM2 (:) (char '.') (many1 digit)
       return $ if null fractional
           then LiteralInt $ read integer
           else LiteralFloat $ read (integer++fractional)

octToken :: Parsec String u Literal
octToken = try $
    do char '0'
       char 'o' <|> char 'O'
       liftM (LiteralInt .foldl (\c -> (c*8+).digitToInt) 0) $ many1 octDigit

hexToken :: Parsec String u Literal
hexToken = try $
    do char '0'
       char 'x' <|> char 'X'
       liftM (LiteralInt .foldl (\c -> (c*16+).digitToInt) 0) $ many1 hexDigit

strElem :: Parsec String u Char
strElem = noneOf "\\\"\'" <|> liftM2 (\bs -> conv.(`const` bs)) (char '\\') (oneOf "abfnrtv\\\"\'")
    where conv c = fromMaybe c $ lookup c
              [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t'),('v','\v')]

stringToken :: Parsec String u Literal
stringToken = between (string "\"") (string "\"") $ liftM LiteralString $ many strElem

charToken :: Parsec String u Literal
charToken = between (string "\'") (string "\'") $ liftM LiteralChar strElem

numLiteralToken :: LayoutInfo -> Parsec String u (PlusPos Literal)
numLiteralToken layout = liftM3 (\a b c -> PlusPos a b)
    (getPosWithTest layout) (hexToken <|> octToken <|> decToken) spacesAndComments

literalToken :: LayoutInfo -> Parsec String u (PlusPos Literal)
literalToken layout = liftM3 (\a b c -> PlusPos a b) (getPosWithTest layout)
    (hexToken <|> octToken <|> decToken <|> stringToken <|> charToken) spacesAndComments

-- Module Name Parser

moduleNameParser :: Parsec String u [String]
moduleNameParser = sepBy1 (liftM2 (:) upper (many $ alphaNum <|> oneOf "_\'")) (char '.')

namespaceParser :: LayoutInfo -> Parsec String u [String]
namespaceParser layout = let tlayout = tailElemLayout layout in
    option [] $ do lexer layout (string "#")
                   (PlusPos _ modname) <- lexer tlayout moduleNameParser
                   return modname

-- Expression Parser

exprParser :: Int -> LayoutInfo -> Parsec String u Expr
exprParser 0 = exprWithTypeParser
exprParser 1 = opExprParser
exprParser 2 = choice.flip amap [lambdaExprParser,exprParser 3]
exprParser 3 = choice.flip amap [letParser,ifParser,caseParser,exprParser 4]
exprParser 4 = applyParser
exprParser 5 = choice.flip amap [nameParser,literalParser,bracketParser,listParser]

exprWithTypeParser layout = let tlayout = tailElemLayout layout in
    do expr@(Expr pos _) <- exprParser 1 layout
       do lexer tlayout (keysymbol "::")
          typeName <- typeNameParaser tlayout
          return $ exprWithType pos expr typeName
          <|> return expr

lambdaExprParser :: LayoutInfo -> Parsec String u Expr
lambdaExprParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "\\")
       body <- sepBy1 (oneLambda tlayout) (lexer tlayout (string "|"))
       return $ lambdaExpr pos body
    where
        oneLambda :: LayoutInfo -> Parsec String u Lambda
        oneLambda layout = let tlayout = tailElemLayout layout in
            do params@(PatternMatch pos _:_) <- manyPatternParser layout
               lexer tlayout (keysymbol "->")
               expr <- exprParser 0 tlayout
               return $ Lambda pos params expr

opExprParser :: LayoutInfo -> Parsec String u Expr
opExprParser layout = let tlayout = tailElemLayout layout in
    do head@(Expr pos _) <- exprParser 2 layout
       do (PlusPos _ op) <- opToken tlayout
          namespace <- namespaceParser tlayout
          tail <- opExprParser tlayout
          return $ infixExpr pos op namespace head tail
          <|> return head
    <|> liftM2 (\(PlusPos pos _) -> negativeExpr pos)
            (lexer layout $ keysymbol "-") (opExprParser tlayout)

nameParser :: LayoutInfo -> Parsec String u Expr
nameParser layout = let tlayout = tailElemLayout layout in
    liftM2 (\(PlusPos pos body) namespace -> nameExpr pos body namespace)
        (nameToken layout) (namespaceParser tlayout)

literalParser :: LayoutInfo -> Parsec String u Expr
literalParser layout =
    liftM (\(PlusPos pos literal) -> literalExpr pos literal) (literalToken layout)

bracketParser :: LayoutInfo -> Parsec String u Expr
bracketParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "(")
       body <- exprParser 0 tlayout
       lexer tlayout (string ")")
       return $ bracketExpr pos body

listParser :: LayoutInfo -> Parsec String u Expr
listParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "[")
       body <- sepBy (exprParser 0 tlayout) (lexer tlayout (string ","))
       lexer tlayout (string "]")
       return $ listExpr pos body

letParser :: LayoutInfo -> Parsec String u Expr
letParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "let")
       list <- layoutMany1 tlayout let1Parser
       lexer tlayout (keyword "in")
       expr <- exprParser 0 tlayout
       return $ letExpr pos list expr
    where
        let1Parser layout = let tlayout = tailElemLayout layout in
            do pattern <- aPatternParser layout
               lexer tlayout (keysymbol "=")
               expr <- exprParser 0 tlayout
               return $ LetOne pattern expr

ifParser :: LayoutInfo -> Parsec String u Expr
ifParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "if")
       c <- exprParser 0 tlayout
       lexer tlayout (keyword "then")
       t <- exprParser 0 tlayout
       lexer tlayout (keyword "else")
       f <- exprParser 0 tlayout
       return $ ifExpr pos c t f

caseParser :: LayoutInfo -> Parsec String u Expr
caseParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (keyword "case")
       expr <- exprParser 0 tlayout
       lexer tlayout (keyword "of")
       list <- layoutMany1 tlayout casePatternParser
       return $ caseExpr pos expr list
    where
        casePatternParser layout = let tlayout = tailElemLayout layout in
            do pattern <- aPatternParser layout
               do lexer tlayout (keysymbol "->")
                  exprParser 0 tlayout >>= return.CasePattern pattern.Left
                  <|> (layoutMany1 tlayout caseGuardParser >>= return.CasePattern pattern.Right)
        caseGuardParser layout = let tlayout = tailElemLayout layout in
            do lexer layout (string "|")
               cond <- exprParser 0 tlayout
               lexer tlayout (keysymbol "->")
               expr <- exprParser 0 tlayout
               return $ Guard cond expr

applyParser layout = let tlayout = tailElemLayout layout in
    liftM2 (foldl apply) (exprParser 5 layout) $ many $ exprParser 5 tlayout
    where apply l@(Expr pos _) r = applyFunctionExpr pos l r

-- Pattern Match Parser

aPatternParser :: LayoutInfo -> Parsec String u PatternMatch
aPatternParser = patternParser 0

manyPatternParser :: LayoutInfo -> Parsec String u [PatternMatch]
manyPatternParser layout = let tlayout = tailElemLayout layout in
    do liftM2 (:) (patternParser 3 layout) (many $ patternParser 3 tlayout)

patternParser :: Int -> LayoutInfo -> Parsec String u PatternMatch
patternParser 0 = patternWithTypeParser
patternParser 1 = opPatternParser
patternParser 2 = choice.flip amap [dcPatternParser,patternParser 3]
patternParser 3 = choice.flip amap [literalPatternParser,asPatternParser,
    bracketPatternParser,listPatternParser,singleDCPatternParser]

patternWithTypeParser :: LayoutInfo -> Parsec String u PatternMatch
patternWithTypeParser layout = let tlayout = tailElemLayout layout in
    do head@(PatternMatch pos _) <- patternParser 1 layout
       do lexer tlayout (keysymbol "::")
          typeName <- typeNameParaser tlayout
          return $ patternWithType pos head typeName
          <|> return head

opPatternParser :: LayoutInfo -> Parsec String u PatternMatch
opPatternParser layout = let tlayout = tailElemLayout layout in
    do head@(PatternMatch pos _) <- patternParser 2 layout
       do (PlusPos _ cons) <- cOpToken tlayout <|> lexer tlayout (keysymbol "+")
          namespace <- namespaceParser tlayout
          tail <- opPatternParser tlayout
          return $ dcOpPattern pos cons namespace head tail
          <|> return head
    <|> do (PlusPos pos _) <- lexer layout (keysymbol "-")
           (PlusPos _ num) <- numLiteralToken tlayout
           return $ case num of
               (LiteralInt n) -> literalPattern pos $ LiteralInt $ -n
               (LiteralFloat n) -> literalPattern pos $ LiteralFloat $ -n

dcPatternParser :: LayoutInfo -> Parsec String u PatternMatch
dcPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos cons) <- cNameToken layout
       namespace <- namespaceParser tlayout
       body <- many (patternParser 3 (tailElemLayout tlayout))
       return $ dcPattern pos cons namespace body

literalPatternParser :: LayoutInfo -> Parsec String u PatternMatch
literalPatternParser layout =
    literalToken layout >>= \(PlusPos pos literal) -> return $ literalPattern pos literal

asPatternParser :: LayoutInfo -> Parsec String u PatternMatch
asPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos var) <- vNameToken layout
       do lexer tlayout (keysymbol "@")
          pattern <- patternParser 2 tlayout
          return $ asPattern pos var pattern
          <|> (return $ bindPattern pos var)

singleDCPatternParser :: LayoutInfo -> Parsec String u PatternMatch
singleDCPatternParser layout = let tlayout = tailElemLayout layout in
    liftM2 (\(PlusPos pos cons) namespace -> dcPattern pos cons namespace [])
        (cNameToken layout) (namespaceParser tlayout)

bracketPatternParser :: LayoutInfo -> Parsec String u PatternMatch
bracketPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "(")
       body <- patternParser 0 tlayout
       lexer tlayout (string ")")
       return $ bracketPattern pos body

listPatternParser :: LayoutInfo -> Parsec String u PatternMatch
listPatternParser layout = let tlayout = tailElemLayout layout in
    do (PlusPos pos _) <- lexer layout (string "[")
       body <- sepBy (patternParser 0 tlayout) (lexer tlayout (string ","))
       lexer tlayout (string "]")
       return $ listPattern pos body

-- Type Name Parser

typeNameParaser :: LayoutInfo -> Parsec String u DataType
typeNameParaser layout = let tlayout = tailElemLayout layout in
    liftM3 DataType getPos
        (option [] $ try $ typeClassInfoParser layout)
        (primTypeParser 0 tlayout)

typeClassInfoParser :: LayoutInfo -> Parsec String u [TypeClassInfo]
typeClassInfoParser layout = let tlayout = tailElemLayout layout in
    do lexer layout (string "(")
       body <- sepBy1 (liftM2 TypeClassInfo (liftM minusPos $ cNameToken tlayout)
           (primTypeParser 0 tlayout)) (lexer tlayout (string ","))
       lexer tlayout (string ")")
       lexer tlayout (keysymbol "=>")
       return body

primTypeParser :: Int -> LayoutInfo -> Parsec String u PrimDataType
primTypeParser 0 = functionTypeParser
primTypeParser 1 = choice.flip amap [composedTypeParser,primTypeParser 2]
primTypeParser 2 = choice.flip amap [composedTypeParser1,listTypeParser,bracketTypeParser]

functionTypeParser :: LayoutInfo -> Parsec String u PrimDataType
functionTypeParser layout = let tlayout = tailElemLayout layout in
    chainr1 (primTypeParser 1 layout) $ liftM (const FunctionType) (lexer tlayout (keysymbol "->"))

composedTypeParser :: LayoutInfo -> Parsec String u PrimDataType
composedTypeParser layout = let tlayout = tailElemLayout layout in
    liftM3 ComposedDataType (liftM minusPos $ cNameToken layout)
        (namespaceParser tlayout) (many $ primTypeParser 2 tlayout)

composedTypeParser1 :: LayoutInfo -> Parsec String u PrimDataType
composedTypeParser1 layout = let tlayout = tailElemLayout layout in
    liftM3 ComposedDataType (liftM minusPos $ cNameToken layout)
        (namespaceParser tlayout) (return [])

listTypeParser :: LayoutInfo -> Parsec String u PrimDataType
listTypeParser layout = let tlayout = tailElemLayout layout in
    do lexer layout (string "[")
       body <- primTypeParser 0 tlayout
       lexer tlayout (string "]")
       return $ ListType body

bracketTypeParser :: LayoutInfo -> Parsec String u PrimDataType
bracketTypeParser layout = let tlayout = tailElemLayout layout in
    do lexer layout (string "(")
       body <- primTypeParser 0 tlayout
       lexer tlayout (string ")")
       return body

-- Global Parser

globalParser :: Parsec String u Expr
globalParser =
    do lexer arbitraryLayout (return "")
       result <- exprParser 0 arbitraryLayout
       eofToken arbitraryLayout
       return result

