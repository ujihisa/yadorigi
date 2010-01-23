
module Yadorigi.Parser.Parser where

import Yadorigi.Common
import Yadorigi.Parser.DataTypes

import Text.Parsec

import Control.Applicative ((<$>))
import Control.Monad

import Data.Char
import Data.Maybe

-- Position

getPos :: Parsec s u Position
getPos = (\p -> Position (sourceLine p) (sourceColumn p)) <$> getPosition

testPos :: LayoutInfo -> Position -> Parsec s u ()
testPos layout pos | checkLayout layout pos = return ()
                   | otherwise = fail "Layout error"

getPosWithTest :: LayoutInfo -> Parsec s u Position
getPosWithTest layout =
    do pos <- getPos
       testPos layout pos >> return pos

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

-- Primitive Parser

getToken :: (Token -> Maybe a) -> LayoutInfo -> Parsec TokenStream u a
getToken f layout =
    do getPosWithTest layout
       token show (\(Token' pos _) -> pos) (\(Token' _ token) -> f token)

emptyToken :: LayoutInfo -> Parsec TokenStream u ()
emptyToken = getToken (guard.(== EmptyToken))

literalToken :: LayoutInfo -> Parsec TokenStream u Literal
literalToken = getToken f
    where f (LiteralToken literal) = Just literal
          f _ = Nothing

numLiteralToken :: LayoutInfo -> Parsec TokenStream u Literal
numLiteralToken = getToken f
    where f (LiteralToken (LiteralInt n)) = Just (LiteralInt n)
          f (LiteralToken (LiteralFloat n)) = Just (LiteralFloat n)
          f _ = Nothing

nameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
nameToken = getToken f
    where f (NameToken name) = Just name
          f _ = Nothing

cNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cNameToken = getToken f
    where f (NameToken name@(ScopedName _ str))
              | isUpper (head str) = Just name
              | otherwise = Nothing
          f _ = Nothing

vNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameToken = getToken f
    where f (NameToken name@(ScopedName _ str))
              | isLower (head str) || '_' == (head str) = Just name
              | otherwise = Nothing
          f _ = Nothing

unscopedNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedNameToken = getToken f
    where f (NameToken (ScopedName [] str))
              | isLower (head str) || '_' == (head str) = Just str
              | otherwise = Nothing
          f _ = Nothing

opToken :: LayoutInfo -> Parsec TokenStream u ScopedName
opToken = getToken f
    where f (OpToken name) = Just name
          f _ = Nothing

cOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpToken = getToken f
    where f (OpToken op@(ScopedName _ str))
              | head str == ':'  = Just op
              | otherwise = Nothing
          f _ = Nothing

vOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpToken = getToken f
    where f (OpToken op@(ScopedName _ str))
              | head str /= ':'  = Just op
              | otherwise = Nothing
          f _ = Nothing

fixedOpToken :: String -> LayoutInfo -> Parsec TokenStream u ScopedName
fixedOpToken str = getToken f
    where f (OpToken op@(ScopedName _ str'))
              | str == str' = Just op
              | otherwise = Nothing
          f _ = Nothing

reservedToken :: String -> LayoutInfo -> Parsec TokenStream u Token
reservedToken s = getToken f
    where f t@(ReservedToken s') | s == s' = Just t
                                 | otherwise = Nothing
          f _ = Nothing

-- Parser Combinators

layoutMany :: LayoutInfo -> (LayoutInfo -> Parsec [s] u a) -> Parsec [s] u [a]
layoutMany layout parser =
    getPosWithTest layout >>= (\(Position _ col) -> many $ parser $ LayoutInfo True col)

layoutMany1 :: LayoutInfo -> (LayoutInfo -> Parsec [s] u a) -> Parsec [s] u [a]
layoutMany1 layout parser =
    getPosWithTest layout >>= (\(Position _ col) -> many1 $ parser $ LayoutInfo True col)

-- Expression Parser

exprParser :: Int -> LayoutInfo -> Parsec TokenStream u Expr
exprParser 0 = exprWithTypeParser
exprParser 1 = opExprParser
exprParser 2 = choice.flip amap [lambdaExprParser,exprParser 3]
exprParser 3 = choice.flip amap [letParser,ifParser,caseParser,exprParser 4]
exprParser 4 = applyParser
exprParser 5 = choice.flip amap [nameParser,literalParser,bracketParser,listParser]

guardParser :: String -> LayoutInfo -> Parsec TokenStream u Guard
guardParser str layout = let tlayout = tailElemLayout layout in
    do reservedToken "|" layout
       cond <- exprParser 0 tlayout
       reservedToken str tlayout
       expr <- exprParser 0 tlayout
       return $ Guard cond expr

exprOrGuardParser :: String -> LayoutInfo -> Parsec TokenStream u ExprOrGuard
exprOrGuardParser str layout = let tlayout = tailElemLayout layout in
    (reservedToken str layout >> (Left <$> exprParser 0 tlayout)) <|>
        (Right <$> layoutMany1 layout (guardParser str))

exprWithTypeParser :: LayoutInfo -> Parsec TokenStream u Expr
exprWithTypeParser layout = let tlayout = tailElemLayout layout in
    do expr@(Expr pos _) <- exprParser 1 layout
       do reservedToken "::" tlayout
          typeName <- typeNameParaser tlayout
          return $ exprWithType pos expr typeName
          <|> return expr

opExprParser :: LayoutInfo -> Parsec TokenStream u Expr
opExprParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       head <- exprParser 2 layout
       do op <- opToken tlayout
          tail <- opExprParser tlayout
          return $ infixExpr pos op head tail
          <|> return head
    <|> liftM3 (\pos _ body -> negativeExpr pos body)
            getPos (fixedOpToken "-" layout) (opExprParser tlayout)

lambdaExprParser :: LayoutInfo -> Parsec TokenStream u Expr
lambdaExprParser layout =
    do pos <- getPos
       reservedToken "\\" layout
       body <- sepBy1 oneLambda (reservedToken "|" tlayout)
       return $ lambdaExpr pos body
    where
        tlayout = tailElemLayout layout
        oneLambda =
            do pos <- getPos
               params <- many1PatternParser tlayout
               reservedToken "->" tlayout
               expr <- exprParser 0 tlayout
               return $ Lambda pos params expr

nameParser :: LayoutInfo -> Parsec TokenStream u Expr
nameParser layout = liftM2 nameExpr (getPosWithTest layout) (nameToken layout)

literalParser :: LayoutInfo -> Parsec TokenStream u Expr
literalParser layout = liftM2 literalExpr (getPosWithTest layout) (literalToken layout)

bracketParser :: LayoutInfo -> Parsec TokenStream u Expr
bracketParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "(" layout
       body <- exprParser 0 tlayout
       reservedToken ")" tlayout
       return $ bracketExpr pos body

listParser :: LayoutInfo -> Parsec TokenStream u Expr
listParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "[" layout
       body <- sepBy (exprParser 0 tlayout) (reservedToken "," tlayout)
       reservedToken "]" tlayout
       return $ listExpr pos body

letParser :: LayoutInfo -> Parsec TokenStream u Expr
letParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "let" layout
       list <- layoutMany1 tlayout let1Parser
       reservedToken "in" tlayout
       expr <- exprParser 0 tlayout
       return $ letExpr pos list expr
    where
        let1Parser layout = let tlayout = tailElemLayout layout in
            liftM2 PrimLet (aPatternParser layout) (exprOrGuardParser "=" tlayout)

ifParser :: LayoutInfo -> Parsec TokenStream u Expr
ifParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "if" layout
       c <- exprParser 0 tlayout
       reservedToken "then" tlayout
       t <- exprParser 0 tlayout
       reservedToken "else" tlayout
       f <- exprParser 0 tlayout
       return $ ifExpr pos c t f

caseParser :: LayoutInfo -> Parsec TokenStream u Expr
caseParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "case" layout
       expr <- exprParser 0 tlayout
       reservedToken "of" tlayout
       list <- layoutMany1 tlayout casePatternParser
       return $ caseExpr pos expr list
    where
        casePatternParser layout = let tlayout = tailElemLayout layout in
            liftM2 CasePattern (aPatternParser layout) (exprOrGuardParser "->" tlayout)

applyParser layout = let tlayout = tailElemLayout layout in
    liftM2 (foldl apply) (exprParser 5 layout) (many (exprParser 5 tlayout))
    where apply l@(Expr pos _) r = applyFunctionExpr pos l r

-- Pattern Match Parser

aPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
aPatternParser = patternParser 0

many1PatternParser :: LayoutInfo -> Parsec TokenStream u [PatternMatch]
many1PatternParser layout = let tlayout = tailElemLayout layout in
    do liftM2 (:) (patternParser 3 layout) (many $ patternParser 3 tlayout)

patternParser :: Int -> LayoutInfo -> Parsec TokenStream u PatternMatch
patternParser 0 = patternWithTypeParser
patternParser 1 = opPatternParser
patternParser 2 = choice.flip amap [dcPatternParser,patternParser 3]
patternParser 3 = choice.flip amap [literalPatternParser,asPatternParser,
    bracketPatternParser,listPatternParser,singleDCPatternParser]

patternWithTypeParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
patternWithTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       head <- patternParser 1 layout
       do reservedToken "::" tlayout
          typeName <- typeNameParaser tlayout
          return $ patternWithType pos head typeName
          <|> return head

opPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
opPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       do head <- patternParser 2 layout
          do cons <- cOpToken tlayout <|> fixedOpToken "+" tlayout
             tail <- opPatternParser tlayout
             return $ dcOpPattern pos cons head tail
             <|> return head
        <|> do fixedOpToken "-" layout
               num <- numLiteralToken tlayout
               return $ literalPattern pos $ case num of
                   (LiteralInt n) -> LiteralInt $ -n
                   (LiteralFloat n) -> LiteralFloat $ -n

dcPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
dcPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       cons <- cNameToken layout
       body <- many (patternParser 3 tlayout)
       return $ dcPattern pos cons body

literalPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
literalPatternParser layout =
    liftM2 literalPattern getPos (literalToken layout)

asPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
asPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       var <- unscopedNameToken layout
       do reservedToken "@" tlayout
          pattern <- patternParser 2 tlayout
          return $ asPattern pos var pattern
          <|> return (bindPattern pos var)

singleDCPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
singleDCPatternParser layout = liftM3 dcPattern getPos (cNameToken layout) (return [])

bracketPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
bracketPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "(" layout
       body <- patternParser 0 tlayout
       reservedToken ")" tlayout
       return $ bracketPattern pos body

listPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
listPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "[" layout
       body <- sepBy (patternParser 0 tlayout) (reservedToken "," tlayout)
       reservedToken "]" tlayout
       return $ listPattern pos body

-- Type Name Parser

typeNameParaser :: LayoutInfo -> Parsec TokenStream u DataType
typeNameParaser layout = let tlayout = tailElemLayout layout in
    liftM3 DataType getPos
        (option [] $ try $ typeClassInfoParser layout)
        (primTypeParser 0 tlayout)

typeClassInfoParser :: LayoutInfo -> Parsec TokenStream u [TypeClassInfo]
typeClassInfoParser layout = let tlayout = tailElemLayout layout in
    do reservedToken "(" layout
       body <- sepBy1 (liftM2 TypeClassInfo (cNameToken tlayout) (primTypeParser 0 tlayout))
           (reservedToken "," tlayout)
       reservedToken ")" tlayout
       reservedToken "=>" tlayout
       return body

primTypeParser :: Int -> LayoutInfo -> Parsec TokenStream u PrimDataType
primTypeParser 0 = functionTypeParser
primTypeParser 1 = choice.flip amap [composedTypeParser,primTypeParser 2]
primTypeParser 2 = choice.flip amap [composedTypeParser1,listTypeParser,bracketTypeParser]

functionTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
functionTypeParser layout = let tlayout = tailElemLayout layout in
    chainr1 (primTypeParser 1 layout) $ reservedToken "->" tlayout >> return FunctionType

composedTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
composedTypeParser layout = let tlayout = tailElemLayout layout in
    liftM2 ComposedDataType (cNameToken layout) (many $ primTypeParser 2 tlayout)

composedTypeParser1 :: LayoutInfo -> Parsec TokenStream u PrimDataType
composedTypeParser1 layout = let tlayout = tailElemLayout layout in
    liftM2 ComposedDataType (cNameToken layout) (return [])

listTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
listTypeParser layout = let tlayout = tailElemLayout layout in
    do reservedToken "[" layout
       body <- primTypeParser 0 tlayout
       reservedToken "]" tlayout
       return $ ListType body

bracketTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
bracketTypeParser layout = let tlayout = tailElemLayout layout in
    do reservedToken "(" layout
       body <- primTypeParser 0 tlayout
       reservedToken ")" tlayout
       return body

-- Global Parser

globalParser :: Parsec TokenStream u Expr
globalParser =
    do emptyToken arbitraryLayout
       result <- exprParser 0 arbitraryLayout
       eof
       return result

