
module Yadorigi.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char

-- Data Types

data Position = Position Int Int

data PlusPos body = PlusPos Position body

data LayoutInfo = LayoutInfo Bool Int


data Literal = LiteralInt Int | LiteralFloat Float | LiteralChar Char | LiteralString String

data PatternMatch = PatternMatch Position PrimPatternMatch

data PrimPatternMatch
    = DCPrimPattern String [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern String PatternMatch PatternMatch {- infix data constructor pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch)
        {- bind pattern (including wild card pattern and as pattern) -}
    | BracketPrimPattern PatternMatch {- Bracket Pattern -}

data LetOne = LetOne PatternMatch Expr

data CaseGuard = CaseGuard Expr Expr

data CasePattern = CasePattern PatternMatch (Either Expr [CaseGuard])

data Expr = Expr Position PrimExpr

data PrimExpr
    = LiteralPrimExpr Literal {- literal expression -}
    | NamePrimExpr String {- name expression -}
    | ApplyFunctionPrimExpr Expr Expr {- apply function expression -}
    | InfixPrimExpr String Expr Expr {- infix expression -}
    | NegativePrimExpr Expr {- negative expression -}
    | BracketPrimExpr Expr {- bracket expression -}
    | ListPrimExpr [Expr] {- list expression -}
    | LetPrimExpr [LetOne] Expr {- let expression -}
    | IfPrimExpr Expr Expr Expr {- if expression -}
    | CasePrimExpr Expr [CasePattern] {- case Expression -}

-- Output Format

instance (Show body) => Show (PlusPos body) where
    show (PlusPos (Position line column) body) = show line++","++show column++" "++show body

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

instance Show PatternMatch where
    show (PatternMatch pos pattern) = show pattern

instance Show PrimPatternMatch where
    show (DCPrimPattern str list) = str++concatMap ((' ':).show) list
    show (LiteralPrimPattern literal) = show literal
    show (DCOpPrimPattern str expr1 expr2) = "{"++str++" "++show expr1++" "++show expr2++"}"
    show (ListPrimPattern list) = show list
    show (BindPrimPattern str Nothing) = str
    show (BindPrimPattern str (Just pattern)) = str++"@"++show pattern
    show (BracketPrimPattern pattern) = "("++show pattern++")"

instance Show LetOne where
    show (LetOne pattern expr) = show pattern++" = "++show expr

instance Show CaseGuard where
    show (CaseGuard cond expr) = "| "++show cond++" "++show expr

instance Show CasePattern where
    show (CasePattern pattern (Left expr)) = show pattern++" "++show expr
    show (CasePattern pattern (Right list)) = show pattern++" "++show list

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name) = name
    show (ApplyFunctionPrimExpr func param) = show func++" "++show param
    show (InfixPrimExpr str expr1 expr2) = "{"++str++" "++show expr1++" "++show expr2++"}"
    show (NegativePrimExpr expr) = "-"++show expr
    show (BracketPrimExpr expr) = "("++show expr++")"
    show (ListPrimExpr list) = show list
    show (LetPrimExpr list expr) = "{let "++show list++" "++show expr++"}"
    show (IfPrimExpr c t f) = "{if "++show c++" "++show t++" "++show f++"}"
    show (CasePrimExpr expr list) = "{case "++show expr++" "++show list++"}"

-- Composed Data Constructors

literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> String -> Expr
nameExpr pos = Expr pos.NamePrimExpr

applyFunctionExpr :: Position -> Expr -> Expr -> Expr
applyFunctionExpr pos func = Expr pos.ApplyFunctionPrimExpr func

infixExpr :: Position -> String -> Expr -> Expr -> Expr
infixExpr pos str expr = Expr pos.InfixPrimExpr str expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

bracketExpr :: Position -> Expr -> Expr
bracketExpr pos = Expr pos.BracketPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

letExpr :: Position -> [LetOne] -> Expr -> Expr
letExpr pos list = Expr pos.LetPrimExpr list

ifExpr :: Position -> Expr -> Expr -> Expr -> Expr
ifExpr pos c t = Expr pos.IfPrimExpr c t

caseExpr :: Position -> Expr -> [CasePattern] -> Expr
caseExpr pos expr = Expr pos.CasePrimExpr expr


dcPattern :: Position -> String -> [PatternMatch] -> PatternMatch
dcPattern pos str = PatternMatch pos.DCPrimPattern str

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> String -> PatternMatch -> PatternMatch -> PatternMatch
dcOpPattern pos str pat = PatternMatch pos.DCOpPrimPattern str pat

listPattern :: Position -> [PatternMatch] -> PatternMatch
listPattern pos = PatternMatch pos.ListPrimPattern

bindPattern :: Position -> String -> PatternMatch
bindPattern pos str = PatternMatch pos $ BindPrimPattern str Nothing

asPattern :: Position -> String -> PatternMatch -> PatternMatch
asPattern pos str = PatternMatch pos.BindPrimPattern str.Just

bracketPattern :: Position -> PatternMatch -> PatternMatch
bracketPattern pos = PatternMatch pos.BracketPrimPattern

-- Constant Values

reservedWord :: [String]
reservedWord = ["if","then","else","case","of","let","in"]

reservedSymbol :: [String]
reservedSymbol = ["=","@","->"]

-- Position

getPos :: GenParser tok st Position
getPos = liftM (\p -> Position (sourceLine p) (sourceColumn p)) getPosition

getPosWithTest :: (Show tok) => LayoutInfo -> GenParser tok st Position
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

testPos :: (Show tok) => LayoutInfo -> Position -> GenParser tok st Position
testPos layout pos
    | checkLayout layout pos = return pos
    | otherwise = pzero

keyword :: String -> CharParser st String
keyword str = try $ liftM2 const (string str) $ notFollowedBy $ alphaNum <|> char '_'

keysymbol :: String -> CharParser st String
keysymbol str = try $ liftM2 const (string str) $ notFollowedBy $ oneOf "!#$%&*+-./:<=>?@^"

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
    where { conv 'a' = '\a' ; conv 'b' = '\b' ; conv 'f' = '\f' ; conv 'n' = '\n' ;
            conv 'r' = '\r' ; conv 't' = '\t' ; conv 'v' = '\v' ; conv c = c }

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
    liftM2 (foldl apply) (exprParser 3 layout) (many $ exprParser 3 tlayout)
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
            (lexer layout (keysymbol "-")) (opExprParser tlayout)

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
       (Position _ col) <- getPosWithTest tlayout
       list <- many1 $ let1Parser $ LayoutInfo True col
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
       (Position _ col) <- getPosWithTest tlayout
       list <- many1 $ casePatternParser $ LayoutInfo True col
       return $ caseExpr pos expr list
    where
        casePatternParser layout = let tlayout = tailElemLayout layout in
            do pattern <- patternParser 11 layout
               (Position _ col) <- getPosWithTest tlayout
               do lexer tlayout (keysymbol "->")
                  exprParser 0 tlayout >>= return.CasePattern pattern.Left
                  <|> (many1 (caseGuardParser (LayoutInfo True col))
                      >>= return.CasePattern pattern.Right)
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

