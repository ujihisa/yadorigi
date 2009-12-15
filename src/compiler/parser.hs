
import Text.ParserCombinators.Parsec

-- Data Types

type Position = (Int,Int)

data PState = PState Position

data PlusPos body = PlusPos Position body


data OperatorAssocInfo = Prefix | Infixl | Infix | Infixr

data OperatorInfo = OperatorInfo String OperatorAssocInfo Int


data ConstantValue = ConstantInt Int

data ApplyFunction = ApplyFunction Expr Expr

data Bracket = Bracket Expr

data IfBlock = IfBlock Expr Expr Expr

data Expr = Expr Position PrimExpr

data PrimExpr
    = AtomicPrimExpr String
    | ApplyFunctionPrimExpr ApplyFunction
    | BracketPrimExpr Bracket
    | ListPrimExpr [Expr]
    | IfPrimExpr IfBlock


data PatternTree
    = DCPattern String [PatternTree] {- data constructor pattern -}
    | ConsPattern PatternTree PatternTree {- construct pattern -}
    | ListPattern [PatternTree] {- list pattern -}
    | NPlusKPattern PatternTree Int {- construct pattern -}
    | BindPattern String {- bind pattern (including wild card) -}
    | ConstantPattern ConstantValue {- constant pattern-}

-- Output Format

instance (Show body) => Show (PlusPos body) where
    show (PlusPos (line,column) body)
        = show line++","++show column++" "++show body

instance Show ApplyFunction where
    show (ApplyFunction func param) = "{"++show func++" "++show param++"}"

instance Show Bracket where
    show (Bracket expr) = "("++show expr++")"

instance Show IfBlock where
    show (IfBlock c t f) = "[if "++show c++" "++show t++" "++show f++"]"

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (AtomicPrimExpr str) = str
    show (ApplyFunctionPrimExpr expr) = show expr
    show (BracketPrimExpr expr) = show expr
    show (ListPrimExpr list) = show list
    show (IfPrimExpr expr) = show expr

-- Composed Data Constructors

atomicExpr :: Position -> String -> Expr
atomicExpr pos = Expr pos.AtomicPrimExpr

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

keyword :: String -> GenParser Char st String
keyword str
    = try (do result <- string str
              notFollowedBy alphaNum
              return result)

-- Constant Values

reservedWord :: [String]
reservedWord
    = ["if","then","else"]

-- Position

nextPos :: String -> Position -> Position
nextPos str pos = foldl countUpPos pos str
    where
        countUpPos (line,column) '\n' = (line+1,0)
        countUpPos (line,column) _ = (line,column+1)

-- Tokenizer

parseToken :: (String -> t) -> (Position -> Bool)
    -> (GenParser Char PState String) -> (GenParser Char PState (PlusPos t))
parseToken conv posTest strParser
    = do (PState currentPos) <- getState
         testPos posTest currentPos
         token <- strParser
         whitespace <- spacesAndComments
         setState (PState (nextPos (token++whitespace) currentPos))
         return (PlusPos currentPos (conv token))

spaceToken :: GenParser Char PState String
spaceToken = many1 space

lineCommentToken :: GenParser Char PState String
lineCommentToken
    = between (string "--") (returnConst () (char '\n') <|>  eof) (many anyChar)
         >>= (return.(++"\n").("--"++))

blockCommentToken :: GenParser Char PState String
blockCommentToken
    = do string "{-"
         body <- manyTill anyChar (try (string "-}"))
         return ("{-"++body++"-}")

spacesAndComments :: GenParser Char PState String
spacesAndComments
    = do body <- many (spaceToken <|> lineCommentToken <|> blockCommentToken)
         return $ concat body

nameToken :: (Position -> Bool) -> GenParser Char PState (PlusPos String)
nameToken posTest = parseToken id posTest $ try
    (do str <- headPlusTail (letter <|> char '_') (alphaNum <|> char '_')
            <|> many1 (oneOf "!#$%&*+-./:<=>?@^")
        (if (elem str reservedWord)
            then unexpected $ show str
            else return str))

numberToken :: (Position -> Bool) -> GenParser Char PState (PlusPos String)
numberToken posTest = parseToken id posTest $
    (try $ do string "0x"
              num <- many1 hexDigit
              return ("0x"++num))
    <|> do integer <- many1 digit
           fractional <- option "" $ headPlusTail (char '.') digit
           return (integer++fractional)

eofToken :: (Position -> Bool) -> GenParser Char PState (PlusPos ())
eofToken posTest = parseToken (const ()) posTest $ returnConst "" eof

-- Expression Parser

exprParser :: [OperatorAssocInfo] -> Int
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
exprParser opinfo 11 headPosTest posTest
    = bracketParser opinfo headPosTest posTest
    <|> listParser opinfo headPosTest posTest
    <|> nameParser opinfo headPosTest posTest
    <|> numberParser opinfo headPosTest posTest
exprParser opinfo 10 headPosTest posTest
    = do result@(Expr pos _) <-
             chainl1 (exprParser opinfo 11 (const True) posTest) (return apply)
         testPos headPosTest pos
         return result
      where
          apply l@(Expr pos _) r = applyFunctionExpr pos $ ApplyFunction l r
exprParser opinfo (-1) headPosTest posTest
    = ifParser opinfo headPosTest posTest
    <|> exprParser opinfo 0 headPosTest posTest
exprParser opinfo n headPosTest posTest
    = exprParser opinfo (n+1) headPosTest posTest

nameParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
nameParser opinfo headPosTest posTest
    = do (PlusPos pos body) <- nameToken headPosTest
         return $ atomicExpr pos body

numberParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
numberParser opinfo headPosTest posTest
    = do (PlusPos pos body) <- numberToken headPosTest
         return $ atomicExpr pos body

listParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
listParser opinfo headPosTest posTest
    = do (PlusPos pos _) <- parseToken (const ()) headPosTest (string "[")
         body <- sepBy (exprParser opinfo (-10) (const True) posTest)
             (parseToken (const ()) posTest (string ","))
         parseToken (const ()) posTest (string "]")
         return $ listExpr pos body

bracketParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
bracketParser opinfo headPosTest posTest
    = do (PlusPos pos _) <- parseToken (const ()) headPosTest (string "(")
         body <- exprParser opinfo (-10) (const True) posTest
         parseToken (const ()) posTest (string ")")
         return $ bracketExpr pos $ Bracket body

ifParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> (Position -> Bool) -> GenParser Char PState Expr
ifParser opinfo headPosTest posTest
    = do parseToken (const ()) headPosTest (keyword "if")
         c@(Expr pos _) <- exprParser opinfo (-1) (const True) posTest
         parseToken (const ()) posTest (keyword "then")
         t <- exprParser opinfo (-1) (const True) posTest
         parseToken (const ()) posTest (keyword "else")
         f <- exprParser opinfo (-1) (const True) posTest
         return $ ifExpr pos $ IfBlock c t f

-- Pattern Match Parser



-- Global Parser

globalParser :: GenParser Char PState Expr
globalParser
    = let posTest = const True in
      do parseToken (const ()) posTest (return "")
         result <- exprParser [] (-10) posTest posTest
         eofToken posTest
         return result

-- Parser Tester

strParser :: String -> IO ()
strParser str
    = case runParser globalParser (PState (0,0)) "<string>" str of
        Left err -> print err
        Right result -> print result

interactiveParser :: IO ()
interactiveParser
    = do input <- getInput
         case runParser globalParser (PState (0,0)) "<interactive>" input of
             Left err -> print err
             Right result -> print result
      where
          getInput = do h <- getLine
                        t <- if h == "" then return "" else getInput
                        return (h++"\n"++t)

fileParser :: FilePath -> IO ()
fileParser filename
    = let myParseFromFile p s filename
              = do input <- readFile filename
                   return (runParser p s filename input) in
      do result <- myParseFromFile
             globalParser (PState (0,0)) filename
         case result of
             Left err -> print err
             Right result -> print result

