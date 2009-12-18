
import Text.ParserCombinators.Parsec
import Data.Char

-- Data Types

data Position = Position Int Int

data PState = PState Position

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

nextPosC :: Char -> Position -> Position
nextPosC '\n' (Position line column) = Position (line+1) 0
nextPosC _ (Position line column) = Position line (column+1)

nextPos :: String -> Position -> Position
nextPos str pos = foldl (flip nextPosC) pos str

updatePosC :: (CharParser PState Char) -> (CharParser PState Char)
updatePosC parser
    = do (PState currentPos) <- getState
         c <- parser
         setState (PState (nextPosC c currentPos))
         return c

updatePos :: (CharParser PState String) -> (CharParser PState String)
updatePos strParser
    = do (PState currentPos) <- getState
         str <- strParser
         setState (PState (nextPos str currentPos))
         return str

-- Layout

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

-- Tokenizer

parseToken :: (String -> t) -> LayoutInfo
    -> (CharParser PState String) -> (CharParser PState (PlusPos t))
parseToken conv layout strParser
    = do (PState currentPos) <- getState
         testPos layout currentPos
         token <- strParser
         whitespace <- spacesAndComments
         setState (PState (nextPos (token++whitespace) currentPos))
         return (PlusPos currentPos (conv token))

spaceToken :: CharParser PState String
spaceToken = many1 space

lineCommentToken :: CharParser PState String
lineCommentToken
    = between (string "--") (returnConst () (char '\n') <|>  eof) (many anyChar)
         >>= (return.("--"++).(++"\n"))

blockCommentToken :: CharParser PState String
blockCommentToken
    = do string "{-"
         body <- manyTill anyChar (try (string "-}"))
         return ("{-" ++ body ++ "-}")

spacesAndComments :: CharParser PState String
spacesAndComments
    = do body <- many (spaceToken <|> lineCommentToken <|> blockCommentToken)
         return $ concat body

labelToken :: (CharParser PState Char) -> (CharParser PState Char)
    -> [String] -> LayoutInfo -> CharParser PState (PlusPos String)
labelToken hparser tparser reservedList layout
    = parseToken id layout $ try
        (do str <- headPlusTail hparser tparser
            (if (elem str reservedList)
                then unexpected $ show str
                else return str))

nameToken :: LayoutInfo -> CharParser PState (PlusPos String)
nameToken layout = labelToken
    (letter <|> char '_') (alphaNum <|> char '_') reservedWord layout

opToken :: LayoutInfo -> CharParser PState (PlusPos String)
opToken layout = labelToken (oneOf "!#$%&*+-./:<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

vNameToken :: LayoutInfo -> CharParser PState (PlusPos String)
vNameToken layout = labelToken
    (lower <|> char '_') (alphaNum <|> char '_') reservedWord layout

vOpToken :: LayoutInfo -> CharParser PState (PlusPos String)
vOpToken layout = labelToken (oneOf "!#$%&*+-./<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

cNameToken :: LayoutInfo -> CharParser PState (PlusPos String)
cNameToken layout = labelToken
    upper (alphaNum <|> char '_') reservedWord layout

cOpToken :: LayoutInfo -> CharParser PState (PlusPos String)
cOpToken layout = labelToken
    (char ':') (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol layout

eofToken :: LayoutInfo -> CharParser PState (PlusPos ())
eofToken layout = parseToken (const ()) layout $ returnConst "" eof

-- Literal Token

numberToken :: CharParser PState (PlusPos Literal)
numberToken
    = do (PState currentPos) <- getState
         integer <- updatePos $ many1 digit
         fractional <- updatePos $ option "" $ headPlusTail (char '.') digit
         return $ PlusPos currentPos $
             if null fractional
                 then LiteralInt $ read integer
                 else LiteralFloat $ read (integer++fractional)

hexToken :: CharParser PState (PlusPos Literal)
hexToken = try $
    do (PState currentPos) <- getState
       updatePos $ string "0x"
       num <- many1 hexnum
       return $ PlusPos currentPos $
           LiteralInt $ foldl (\c n -> c*16+n) 0 num
    where
        hexnum = do c <- updatePosC hexDigit
                    if '0' <= c && c <= '9'
                        then return $ ord c-ord '0'
                        else return $ ord (toLower c)-ord 'a'+10

strElem :: CharParser PState Char
strElem
    = do c <- noneOf "\\\"\'"
         updatePos $ return [c]
         return c
  <|> do char '\\'
         c <- oneOf "abfnrtv\\\"\'"
         updatePos $ return ['\\',c]
         return $ conv c
    where { conv 'a' = '\a' ; conv 'b' = '\b' ; conv 'f' = '\f'
          ; conv 'n' = '\n' ; conv 'r' = '\r' ; conv 't' = '\t'
          ; conv 'v' = '\v' ; conv c = c }

stringToken :: CharParser PState (PlusPos Literal)
stringToken
    = do (PState currentPos) <- getState
         between (updatePos $ string "\"") (updatePos $ string "\"")
             (many strElem >>= \s -> return $
             PlusPos currentPos $ LiteralString s)

charToken :: CharParser PState (PlusPos Literal)
charToken
    = do (PState currentPos) <- getState
         between (updatePos $ string "\'") (updatePos $ string "\'")
             (strElem >>= \c -> return $ PlusPos currentPos $ LiteralChar c)

literalToken :: LayoutInfo -> CharParser PState (PlusPos Literal)
literalToken layout
    = do (PState currentPos) <- getState
         testPos layout currentPos
         literal <- hexToken <|> numberToken <|> stringToken <|> charToken
         parseToken id arbitraryLayout (return "")
         return literal

-- Expression Parser

exprParser :: Int -> LayoutInfo -> CharParser PState Expr
exprParser (-1) layout
    = ifParser layout <|> exprParser 0 layout
exprParser 10 layout
    = do (PState currentPos) <- getState
         testPos layout currentPos
         chainl1 (exprParser 11 (arbitraryElemLayout layout)) (return apply)
    where
        apply l@(Expr pos _) r = applyFunctionExpr pos $ ApplyFunction l r
exprParser 11 layout
    = bracketParser layout <|> listParser layout
    <|> nameParser layout <|> literalParser layout
exprParser n layout
    = exprParser (n+1) layout

nameParser :: LayoutInfo -> CharParser PState Expr
nameParser layout
    = nameToken layout >>= \(PlusPos pos body) -> return $ nameExpr pos body

literalParser :: LayoutInfo -> CharParser PState Expr
literalParser layout = literalToken layout
    >>= \(PlusPos pos literal) -> return $ literalExpr pos literal

listParser :: LayoutInfo -> CharParser PState Expr
listParser layout
    = do (PlusPos pos _) <- parseToken id layout (string "[")
         body <- sepBy (exprParser (-10) (tailElemLayout layout))
             (parseToken id (tailElemLayout layout) (string ","))
         parseToken id (tailElemLayout layout) (string "]")
         return $ listExpr pos body

bracketParser :: LayoutInfo -> CharParser PState Expr
bracketParser layout
    = do (PlusPos pos _) <- parseToken id layout (string "(")
         body <- exprParser (-10) (tailElemLayout layout)
         parseToken id (tailElemLayout layout) (string ")")
         return $ bracketExpr pos $ Bracket body

ifParser :: LayoutInfo -> CharParser PState Expr
ifParser layout
    = do parseToken id layout (keyword "if")
         c@(Expr pos _) <- exprParser (-1) (tailElemLayout layout)
         parseToken id (tailElemLayout layout) (keyword "then")
         t <- exprParser (-1) (tailElemLayout layout)
         parseToken id (tailElemLayout layout) (keyword "else")
         f <- exprParser (-1) (tailElemLayout layout)
         return $ ifExpr pos $ IfBlock c t f

-- Pattern Match Parser

patternParser :: Int -> LayoutInfo -> CharParser PState PatternMatch
patternParser 10 layout
    = asPatternParser layout

literalPatternParser :: LayoutInfo -> CharParser PState PatternMatch
literalPatternParser layout
    = literalToken layout >>= \(PlusPos pos literal)
        -> return $ literalPattern pos literal

asPatternParser :: LayoutInfo -> CharParser PState PatternMatch
asPatternParser layout
    = do (PlusPos pos var) <- vNameToken layout
         do parseToken id (tailElemLayout layout) (string "@")
            pattern <- patternParser 10 (tailElemLayout layout)
            return $ asPattern pos var pattern
           <|> (return $ bindPattern pos var)

-- Global Parser

globalParser :: CharParser PState Expr
globalParser
    = do parseToken id arbitraryLayout (return "")
         result <- exprParser (-10) arbitraryLayout
         eofToken arbitraryLayout
         return result

-- Parser Tester

strParser :: String -> IO ()
strParser input
    = case runParser globalParser (PState (Position 0 0)) "<string>" input of
        Left err -> print err
        Right result -> print result

interactiveParser :: IO ()
interactiveParser
    = do input <- getInput
         case runParser globalParser
                 (PState (Position 0 0)) "<interactive>" input of
             Left err -> print err
             Right result -> print result
    where
        getInput = do h <- getLine
                      t <- if h == "" then return "" else getInput
                      return (h++"\n"++t)

fileParser :: FilePath -> IO ()
fileParser filename
    = do result <- myParseFromFile
             globalParser (PState (Position 0 0)) filename
         case result of
             Left err -> print err
             Right result -> print result
    where
        myParseFromFile p s filename
            = do input <- readFile filename
                 return (runParser p s filename input)

