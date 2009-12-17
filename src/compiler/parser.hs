
import Text.ParserCombinators.Parsec
import Data.Char

-- Data Types

data Position = Position Int Int

data PState = PState Position

data PlusPos body = PlusPos Position body


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


data PatternMatch
    = AsPattern PatternMatch PatternMatch {- as pattern -}
    | DCPattern String [PatternMatch] {- data constructor pattern -}
    | ConsPattern PatternMatch PatternMatch {- construct pattern -}
    | ListPattern [PatternMatch] {- list pattern -}
    | BindPattern String {- bind pattern (including wild card) -}
    | LiteralPattern Literal {- literal pattern -}

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

-- Parser Combinators

headPlusTail :: GenParser tok st a -> GenParser tok st a -> GenParser tok st [a]
headPlusTail head tail
    = do h <- head
         t <- many tail
         return (h:t)

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p
    = p >>= const (return x)

testPos :: (Show tok) => (Position -> Bool) -> Position -> GenParser tok st ()
testPos posTest pos
    = if posTest pos
          then (return ())
          else (do eof >>= const (unexpected "end of file")
                   unexpected "token position")

keyword :: String -> CharParser st String
keyword str
    = try (do result <- string str
              notFollowedBy (alphaNum <|> char '_')
              return result)

-- Constant Values

reservedWord :: [String]
reservedWord
    = ["if","then","else"]

reservedSymbol :: [String]
reservedSymbol
    = ["=","@"]

-- Position

nextPos :: String -> Position -> Position
nextPos str pos = foldl countUpPos pos str
    where
        countUpPos (Position line column) '\n' = Position (line+1) 0
        countUpPos (Position line column) _ = Position line (column+1)

-- Tokenizer

updatePos :: (CharParser PState String) -> (CharParser PState String)
updatePos strParser
    = do (PState currentPos) <- getState
         str <- strParser
         setState (PState (nextPos str currentPos))
         return str

parseToken :: (String -> t) -> (Position -> Bool)
    -> (CharParser PState String) -> (CharParser PState (PlusPos t))
parseToken conv posTest strParser
    = do (PState currentPos) <- getState
         testPos posTest currentPos
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
    -> [String] -> (Position -> Bool) -> CharParser PState (PlusPos String)
labelToken hparser tparser reservedList posTest
    = parseToken id posTest $ try
        (do str <- headPlusTail hparser tparser
            (if (elem str reservedList)
                then unexpected $ show str
                else return str))

nameToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
nameToken posTest = labelToken
    (letter <|> char '_') (alphaNum <|> char '_') reservedWord posTest

opToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
opToken posTest = labelToken (oneOf "!#$%&*+-./:<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol posTest

vNameToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
vNameToken posTest = labelToken
    (lower <|> char '_') (alphaNum <|> char '_') reservedWord posTest

vOpToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
vOpToken posTest = labelToken (oneOf "!#$%&*+-./<=>?@^")
    (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol posTest

cNameToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
cNameToken posTest = labelToken
    upper (alphaNum <|> char '_') reservedWord posTest

cOpToken :: (Position -> Bool) -> CharParser PState (PlusPos String)
cOpToken posTest = labelToken
    (char ':') (oneOf "!#$%&*+-./:<=>?@^") reservedSymbol posTest

eofToken :: (Position -> Bool) -> CharParser PState (PlusPos ())
eofToken posTest = parseToken (const ()) posTest $ returnConst "" eof

-- Literal Token

numberToken :: (Position -> Bool) -> CharParser PState (PlusPos Literal)
numberToken posTest
    = do (PState currentPos) <- getState
         integer <- many1 digit
         fractional <- option "" $ headPlusTail (char '.') digit
         updatePos $ return (integer++fractional)
         return $ PlusPos currentPos $
             if null fractional
                 then LiteralInt $ read integer
                 else LiteralFloat $ read (integer++fractional)

hexToken :: (Position -> Bool) -> CharParser PState (PlusPos Literal)
hexToken posTest
    = try $ do (PState currentPos) <- getState
               updatePos $ string "0x"
               num <- many1 hexnum
               return $ PlusPos currentPos $
                   LiteralInt $ foldl (\c n -> c*16+n) 0 num
    where
        hexnum = do c <- updatePos (hexDigit >>= return.(:[])) >>= (return.head)
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

stringToken :: (Position -> Bool) -> CharParser PState (PlusPos Literal)
stringToken posTest
    = do (PState currentPos) <- getState
         testPos posTest currentPos
         between (updatePos $ string "\"") (updatePos $ string "\"")
             (many strElem >>= \s -> return $
             PlusPos currentPos $ LiteralString s)

charToken :: (Position -> Bool) -> CharParser PState (PlusPos Literal)
charToken posTest
    = do (PState currentPos) <- getState
         testPos posTest currentPos
         between (updatePos $ string "\'") (updatePos $ string "\'")
             (strElem >>= \c -> return $ PlusPos currentPos $ LiteralChar c)

literalToken :: (Position -> Bool) -> CharParser PState (PlusPos Literal)
literalToken posTest
    = do literal <- hexToken posTest <|> numberToken posTest
             <|> stringToken posTest <|> charToken posTest
         parseToken (const ()) (const True) (return "")
         return literal

-- Expression Parser

exprParser :: Int
    -> (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
exprParser (-1) headPosTest posTest
    = ifParser headPosTest posTest <|> exprParser 0 headPosTest posTest
exprParser 10 headPosTest posTest
    = do result@(Expr pos _) <-
             chainl1 (exprParser 11 posTest posTest) (return apply)
         testPos headPosTest pos
         return result
    where
        apply l@(Expr pos _) r = applyFunctionExpr pos $ ApplyFunction l r
exprParser 11 headPosTest posTest
    = bracketParser headPosTest posTest <|> listParser headPosTest posTest
    <|> nameParser headPosTest posTest <|> literalParser headPosTest posTest
exprParser n headPosTest posTest
    = exprParser (n+1) headPosTest posTest

nameParser :: (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
nameParser headPosTest posTest
    = do (PlusPos pos body) <- nameToken headPosTest
         return $ nameExpr pos body

literalParser
    :: (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
literalParser headPosTest posTest
    = do (PlusPos pos literal) <- literalToken headPosTest
         return $ literalExpr pos literal

listParser :: (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
listParser headPosTest posTest
    = do (PlusPos pos _) <- parseToken (const ()) headPosTest (string "[")
         body <- sepBy (exprParser (-10) posTest posTest)
             (parseToken (const ()) posTest (string ","))
         parseToken (const ()) posTest (string "]")
         return $ listExpr pos body

bracketParser
    :: (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
bracketParser headPosTest posTest
    = do (PlusPos pos _) <- parseToken (const ()) headPosTest (string "(")
         body <- exprParser (-10) posTest posTest
         parseToken (const ()) posTest (string ")")
         return $ bracketExpr pos $ Bracket body

ifParser :: (Position -> Bool) -> (Position -> Bool) -> CharParser PState Expr
ifParser headPosTest posTest
    = do parseToken (const ()) headPosTest (keyword "if")
         c@(Expr pos _) <- exprParser (-1) posTest posTest
         parseToken (const ()) posTest (keyword "then")
         t <- exprParser (-1) posTest posTest
         parseToken (const ()) posTest (keyword "else")
         f <- exprParser (-1) posTest posTest
         return $ ifExpr pos $ IfBlock c t f

-- Pattern Match Parser

{-
patternParser :: Int -> (Position -> Bool) -> (Position -> Bool)
    -> CharParser PState PatternMatch
patternParser 0 headPosTest posTest
    = 
patternParser 10 headPosTest posTest
    = asPatternParser headPosTest posTest
    <|> 

asPatternParser :: (Position -> Bool)
    -> (Position -> Bool) -> CharParser PState PatternMatch
asPatternParser headPosTest posTest
    = do var <- vNameToken headPosTest
         parseToken (const ()) posTest (string "@")
         pattern <- patternParser 10 posTest posTest
         return $ AsPattern (BindPattern var) pattern
-}

-- Global Parser

globalParser :: CharParser PState Expr
globalParser
    = let posTest = const True in
      do parseToken (const ()) posTest (return "")
         result <- exprParser (-10) posTest posTest
         eofToken posTest
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

