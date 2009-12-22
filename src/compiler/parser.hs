
import Text.ParserCombinators.Parsec
import Data.Char

-- Data Types

data Position = Position Int Int

data PlusPos body = PlusPos Position body

data LayoutInfo = LayoutInfo Bool Int


data Literal
    = LiteralInt Int
    | LiteralFloat Float
    | LiteralChar Char
    | LiteralString String

data ApplyFunction = ApplyFunction Expr Expr

data Bracket = Bracket Expr

data IfBlock = IfBlock Expr Expr Expr

data Expr = Expr Position PrimExpr

data PrimExpr
    = LiteralPrimExpr Literal
    | NamePrimExpr String
    | ApplyFunctionPrimExpr ApplyFunction
    | InfixPrimExpr String Expr Expr
    | NegativePrimExpr Expr
    | BracketPrimExpr Bracket
    | ListPrimExpr [Expr]
    | IfPrimExpr IfBlock

data PatternMatch = PatternMatch Position PrimPatternMatch

data PrimPatternMatch
    = DCPrimPattern String [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern String PatternMatch PatternMatch
        {- infix data constructor pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch)
        {- bind pattern (including wild card pattern and as pattern) -}
    | BracketPrimPattern PatternMatch {- Bracket Pattern -}

-- Output Format

instance (Show body) => Show (PlusPos body) where
    show (PlusPos (Position line column) body)
        = show line++","++show column++" "++show body

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

instance Show ApplyFunction where
    show (ApplyFunction func param) = "{"++show func++" "++show param++"}"

instance Show Bracket where
    show (Bracket expr) = "("++show expr++")"

instance Show IfBlock where
    show (IfBlock c t f) = "[if "++show c++" "++show t++" "++show f++"]"

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name) = name
    show (ApplyFunctionPrimExpr expr) = show expr
    show (InfixPrimExpr str expr1 expr2)
        = "{"++str++" "++show expr1++" "++show expr2++"}"
    show (NegativePrimExpr expr) = "-"++show expr
    show (BracketPrimExpr expr) = show expr
    show (ListPrimExpr list) = show list
    show (IfPrimExpr expr) = show expr

-- Composed Data Constructors

literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> String -> Expr
nameExpr pos = Expr pos.NamePrimExpr

applyFunctionExpr :: Position -> ApplyFunction -> Expr
applyFunctionExpr pos = Expr pos.ApplyFunctionPrimExpr

infixExpr :: Position -> String -> Expr -> Expr -> Expr
infixExpr pos str expr = Expr pos.InfixPrimExpr str expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

bracketExpr :: Position -> Bracket -> Expr
bracketExpr pos = Expr pos.BracketPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

ifExpr :: Position -> IfBlock -> Expr
ifExpr pos = Expr pos.IfPrimExpr


dcPattern :: Position -> String -> [PatternMatch] -> PatternMatch
dcPattern pos str = PatternMatch pos.DCPrimPattern str

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> String
    -> PatternMatch -> PatternMatch -> PatternMatch
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
reservedWord
    = ["if","then","else"]

reservedSymbol :: [String]
reservedSymbol
    = ["=","@"]

-- Position

getPos :: GenParser tok st Position
getPos = do p <- getPosition
            return $ Position (sourceLine p) (sourceColumn p)

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

headPlusTail :: GenParser tok st a -> GenParser tok st a -> GenParser tok st [a]
headPlusTail head tail
    = do h <- head
         t <- many tail
         return (h:t)

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p
    = p >>= const (return x)

testPos :: (Show tok) => LayoutInfo -> Position -> GenParser tok st ()
testPos layout pos
    = if checkLayout layout pos
          then return ()
          else do eof >>= const (unexpected "end of file")
                  unexpected "token position"

keyword :: String -> CharParser st String
keyword str = try $
    do result <- string str
       notFollowedBy (alphaNum <|> char '_')
       return result

keysymbol :: String -> CharParser st String
keysymbol str = try $
    do result <- string str
       notFollowedBy (oneOf "!#$%&*+-./:<=>?@^")
       return result

-- Tokenizer

parseToken :: LayoutInfo -> (CharParser st t) -> (CharParser st (PlusPos t))
parseToken layout strParser
    = do pos <- getPos
         testPos layout pos
         token <- strParser
         spacesAndComments
         return (PlusPos pos token)


lineCommentToken :: CharParser st ()
lineCommentToken
    = do try $ string "--"
         manyTill anyChar (returnConst () (char '\n') <|>  eof)
         return ()

blockCommentToken :: CharParser st ()
blockCommentToken
    = do try $ string "{-"
         manyTill anyChar (try (string "-}"))
         return ()

spacesAndComments :: CharParser st ()
spacesAndComments
    = skipMany ((space >>= const (return ()))
          <|> lineCommentToken <|> blockCommentToken)

labelToken :: (CharParser () Char) -> (CharParser () Char)
    -> [String] -> LayoutInfo -> CharParser () (PlusPos String)
labelToken hparser tparser reservedList layout
    = parseToken layout $ try
        (do str <- headPlusTail hparser tparser
            (if (elem str reservedList)
                then unexpected $ show str
                else return str))

nameToken :: LayoutInfo -> CharParser () (PlusPos String)
nameToken layout = labelToken
    (letter <|> char '_') (alphaNum <|> char '_') reservedWord layout

opToken :: LayoutInfo -> CharParser () (PlusPos String)
opToken layout = labelToken (oneOf "!#$%&*+-./:<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

vNameToken :: LayoutInfo -> CharParser () (PlusPos String)
vNameToken layout = labelToken
    (lower <|> char '_') (alphaNum <|> char '_') reservedWord layout

vOpToken :: LayoutInfo -> CharParser () (PlusPos String)
vOpToken layout = labelToken (oneOf "!#$%&*+-./<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

cNameToken :: LayoutInfo -> CharParser () (PlusPos String)
cNameToken layout = labelToken
    upper (alphaNum <|> char '_') reservedWord layout

cOpToken :: LayoutInfo -> CharParser () (PlusPos String)
cOpToken layout = labelToken
    (char ':') (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

eofToken :: LayoutInfo -> CharParser () (PlusPos ())
eofToken layout = parseToken layout $ returnConst () eof

-- Literal Token

decToken :: CharParser st Literal
decToken
    = do integer <- many1 digit
         fractional <- option "" $ headPlusTail (char '.') digit
         return $ if null fractional
                      then LiteralInt $ read integer
                      else LiteralFloat $ read (integer++fractional)

octToken :: CharParser st Literal
octToken = try $
    do char '0'
       char 'o' <|> char 'O'
       many1 octDigit >>= return.LiteralInt .foldl (\c -> (c*8+).digitToInt) 0

hexToken :: CharParser st Literal
hexToken = try $
    do char '0'
       char 'x' <|> char 'X'
       many1 hexDigit >>= return.LiteralInt .foldl (\c -> (c*16+).digitToInt) 0

strElem :: CharParser st Char
strElem
    = noneOf "\\\"\'"
    <|> do char '\\'
           oneOf "abfnrtv\\\"\'" >>= return.conv
    where { conv 'a' = '\a' ; conv 'b' = '\b' ; conv 'f' = '\f'
          ; conv 'n' = '\n' ; conv 'r' = '\r' ; conv 't' = '\t'
          ; conv 'v' = '\v' ; conv c = c }

stringToken :: CharParser st Literal
stringToken
    = between (string "\"") (string "\"")
          (many strElem >>= return.LiteralString)

charToken :: CharParser st Literal
charToken
    = between (string "\'") (string "\'") (strElem >>= return.LiteralChar)

numLiteralToken :: LayoutInfo -> CharParser st (PlusPos Literal)
numLiteralToken layout
    = do pos <- getPos
         testPos layout pos
         literal <- hexToken <|> octToken <|> decToken
         spacesAndComments
         return $ PlusPos pos literal

literalToken :: LayoutInfo -> CharParser st (PlusPos Literal)
literalToken layout
    = do pos <- getPos
         testPos layout pos
         literal <- hexToken <|> octToken <|> decToken
             <|> stringToken <|> charToken
         spacesAndComments
         return $ PlusPos pos literal

-- Expression Parser

exprParser :: Int -> LayoutInfo -> CharParser () Expr
exprParser (-1) layout
    = ifParser layout <|> exprParser 0 layout
exprParser 0 layout
    = opExprParser layout
exprParser 10 layout
    = do pos <- getPos
         testPos layout pos
         chainl1 (exprParser 11 (arbitraryElemLayout layout)) (return apply)
    where
        apply l@(Expr pos _) r = applyFunctionExpr pos $ ApplyFunction l r
exprParser 11 layout
    = nameParser layout <|> literalParser layout
    <|> bracketParser layout <|> listParser layout
exprParser n layout
    = exprParser (n+1) layout

opExprParser :: LayoutInfo -> CharParser () Expr
opExprParser layout
    = do head@(Expr pos _) <- exprParser 10 layout
         do (PlusPos _ op) <- opToken (tailElemLayout layout)
            tail <- opExprParser (tailElemLayout layout)
            return $ infixExpr pos op head tail
           <|> return head
    <|> do (PlusPos pos _) <- parseToken layout (keysymbol "-")
           body <- opExprParser (tailElemLayout layout)
           return $ negativeExpr pos body

nameParser :: LayoutInfo -> CharParser () Expr
nameParser layout
    = nameToken layout >>= \(PlusPos pos body) -> return $ nameExpr pos body

literalParser :: LayoutInfo -> CharParser () Expr
literalParser layout = literalToken layout
    >>= \(PlusPos pos literal) -> return $ literalExpr pos literal

bracketParser :: LayoutInfo -> CharParser () Expr
bracketParser layout
    = do (PlusPos pos _) <- parseToken layout (string "(")
         body <- exprParser (-1) (tailElemLayout layout)
         parseToken (tailElemLayout layout) (string ")")
         return $ bracketExpr pos $ Bracket body

listParser :: LayoutInfo -> CharParser () Expr
listParser layout
    = do (PlusPos pos _) <- parseToken layout (string "[")
         body <- sepBy (exprParser (-1) (tailElemLayout layout))
             (parseToken (tailElemLayout layout) (string ","))
         parseToken (tailElemLayout layout) (string "]")
         return $ listExpr pos body

ifParser :: LayoutInfo -> CharParser () Expr
ifParser layout
    = do parseToken layout (keyword "if")
         c@(Expr pos _) <- exprParser (-1) (tailElemLayout layout)
         parseToken (tailElemLayout layout) (keyword "then")
         t <- exprParser (-1) (tailElemLayout layout)
         parseToken (tailElemLayout layout) (keyword "else")
         f <- exprParser (-1) (tailElemLayout layout)
         return $ ifExpr pos $ IfBlock c t f

-- Pattern Match Parser

patternParser :: Int -> LayoutInfo -> CharParser () PatternMatch
patternParser 0 layout
    = opPatternParser layout
patternParser 10 layout
    = patternParser 11 layout <|> dcPatternParser layout
patternParser 11 layout
    = literalPatternParser layout <|> asPatternParser layout
    <|> bracketPatternParser layout <|> listPatternParser layout
    <|> singleDCPatternParser layout

opPatternParser :: LayoutInfo -> CharParser () PatternMatch
opPatternParser layout
    = do head@(PatternMatch pos _) <- patternParser 10 layout
         do (PlusPos _ cons) <- cOpToken (tailElemLayout layout)
                <|> parseToken layout (keysymbol "+")
            tail <- opPatternParser (tailElemLayout layout)
            return $ dcOpPattern pos cons head tail
           <|> return head
    <|> do (PlusPos pos _) <- parseToken layout (keysymbol "-")
           (PlusPos _ num) <- numLiteralToken (tailElemLayout layout)
           return $ case num of
               (LiteralInt n) -> literalPattern pos $ LiteralInt $ -n
               (LiteralFloat n) -> literalPattern pos $ LiteralFloat $ -n

dcPatternParser :: LayoutInfo -> CharParser () PatternMatch
dcPatternParser layout
    = do (PlusPos pos cons) <- cNameToken layout
         body <- many (patternParser 11 layout)
         return $ dcPattern pos cons body

literalPatternParser :: LayoutInfo -> CharParser () PatternMatch
literalPatternParser layout
    = literalToken layout >>= \(PlusPos pos literal)
        -> return $ literalPattern pos literal

asPatternParser :: LayoutInfo -> CharParser () PatternMatch
asPatternParser layout
    = do (PlusPos pos var) <- vNameToken layout
         do parseToken (tailElemLayout layout) (keysymbol "@")
            pattern <- patternParser 10 (tailElemLayout layout)
            return $ asPattern pos var pattern
           <|> (return $ bindPattern pos var)

singleDCPatternParser :: LayoutInfo -> CharParser () PatternMatch
singleDCPatternParser layout
    = do (PlusPos pos cons) <- cNameToken layout
         return $ dcPattern pos cons []

bracketPatternParser :: LayoutInfo -> CharParser () PatternMatch
bracketPatternParser layout
    = do (PlusPos pos _) <- parseToken layout (string "(")
         body <- (patternParser 0 (tailElemLayout layout))
         parseToken layout (string ")")
         return $ bracketPattern pos body

listPatternParser :: LayoutInfo -> CharParser () PatternMatch
listPatternParser layout
    = do (PlusPos pos _) <- parseToken layout (string "[")
         body <- sepBy (patternParser 0 (tailElemLayout layout))
             (parseToken (tailElemLayout layout) (string ","))
         parseToken layout (string "]")
         return $ listPattern pos body

-- Global Parser

globalParser :: CharParser () Expr
globalParser
    = do parseToken arbitraryLayout (return "")
         result <- exprParser (-10) arbitraryLayout
         eofToken arbitraryLayout
         return result

-- Parser Tester

strParser :: String -> IO ()
strParser input
    = case runParser globalParser () "<string>" input of
        Left err -> print err
        Right result -> print result

interactiveParser :: IO ()
interactiveParser
    = do input <- getInput
         case runParser globalParser () "<interactive>" input of
             Left err -> print err
             Right result -> print result
    where
        getInput = do h <- getLine
                      t <- if h == "" then return "" else getInput
                      return (h++"\n"++t)

fileParser :: FilePath -> IO ()
fileParser filename
    = do result <- myParseFromFile globalParser () filename
         case result of
             Left err -> print err
             Right result -> print result
    where
        myParseFromFile p s filename
            = do input <- readFile filename
                 return (runParser p s filename input)

