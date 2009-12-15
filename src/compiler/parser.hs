
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
    | TuplePattern [PatternTree] {- tuple pattern -}
    | ConsPattern PatternTree PatternTree
          {- construct pattern (including list pattern) -}
    | NPlusKPattern PatternTree Int {- construct pattern -}
    | BindPattern String {- bind pattern (including wild card) -}
    | ConstantPattern ConstantValue {- constant pattern-}

-- Composed Constructors

atomicExpr :: Position -> String -> Expr
atomicExpr pos str = Expr pos $ AtomicPrimExpr str

applyFunctionExpr :: Position -> ApplyFunction -> Expr
applyFunctionExpr pos app = Expr pos $ ApplyFunctionPrimExpr app

bracketExpr :: Position -> Bracket -> Expr
bracketExpr pos bracket = Expr pos $ BracketPrimExpr bracket

listExpr :: Position -> [Expr] -> Expr
listExpr pos list = Expr pos $ ListPrimExpr list

ifExpr :: Position -> IfBlock -> Expr
ifExpr pos ifBlock = Expr pos $ IfPrimExpr ifBlock

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

-- Parser Combinators

headPlusTail :: GenParser tok st a -> GenParser tok st a -> GenParser tok st [a]
headPlusTail head tail
    = do h <- head
         t <- many tail
         return (h:t)

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p
    = p >>= const (return x)

testPos :: (Show tok) => (Position -> Bool)
    -> Position -> GenParser tok st a -> GenParser tok st a
testPos ptest pos parser
    = if ptest pos
          then parser
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
parseToken conv ptest strParser
    = do (PState currentPos) <- getState
         testPos ptest currentPos
             (do token <- strParser
                 whitespace <- spacesAndComments
                 setState (PState (nextPos (token++whitespace) currentPos))
                 return (PlusPos currentPos (conv token)))

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
nameToken ptest = parseToken id ptest $ try
    (do str <- headPlusTail (letter <|> char '_') (alphaNum <|> char '_')
            <|> many1 (oneOf "!#$%&*+-./:<=>?@^")
        (if (elem str reservedWord)
            then unexpected $ show str
            else return str))

numberToken :: (Position -> Bool) -> GenParser Char PState (PlusPos String)
numberToken ptest = parseToken id ptest $
    (try $ do string "0x"
              num <- many1 hexDigit
              return ("0x"++num))
    <|> do integer <- many1 digit
           fractional <- option "" $ headPlusTail (char '.') digit
           return (integer++fractional)

eofToken :: (Position -> Bool) -> GenParser Char PState (PlusPos ())
eofToken ptest = parseToken (const ()) ptest $ returnConst "" eof

-- Expression Parser

exprParser :: [OperatorAssocInfo] -> Int
    -> (Position -> Bool) -> GenParser Char PState Expr
exprParser opinfo 11 ptest
    = bracketParser opinfo ptest <|> listParser opinfo ptest
          <|> nameParser opinfo ptest <|> numberParser opinfo ptest
exprParser opinfo 10 ptest
    = chainl1 (exprParser opinfo 11 ptest) (return apply)
      where
          apply l@(Expr pos _) r = applyFunctionExpr pos $ ApplyFunction l r
exprParser opinfo (-1) ptest
    = ifParser opinfo ptest <|> exprParser opinfo 0 ptest
exprParser opinfo n ptest
    = exprParser opinfo (n+1) ptest

nameParser :: [OperatorAssocInfo] -> (Position -> Bool)
    -> GenParser Char PState Expr
nameParser opinfo ptest
    = do (PlusPos pos body) <- nameToken ptest
         return $ atomicExpr pos body

numberParser :: [OperatorAssocInfo] -> (Position -> Bool)
    -> GenParser Char PState Expr
numberParser opinfo ptest
    = do (PlusPos pos body) <- numberToken ptest
         return $ atomicExpr pos body

listParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> GenParser Char PState Expr
listParser opinfo ptest
    = do (PlusPos pos _) <- parseToken (const ()) ptest (string "[")
         body <- sepBy1 (exprParser opinfo (-10) ptest)
             (parseToken (const ()) ptest (string ","))
         parseToken (const ()) ptest (string "]")
         return $ listExpr pos body

bracketParser :: [OperatorAssocInfo]
 -> (Position -> Bool) -> GenParser Char PState Expr
bracketParser opinfo ptest
    = do (PlusPos pos _) <- parseToken (const ()) ptest (string "(")
         body <- exprParser opinfo (-10) ptest
         parseToken (const ()) ptest (string ")")
         return $ bracketExpr pos $ Bracket body

ifParser :: [OperatorAssocInfo]
    -> (Position -> Bool) -> GenParser Char PState Expr
ifParser opinfo ptest
    = do parseToken (const ()) ptest (keyword "if")
         c@(Expr pos _) <- exprParser opinfo (-1) ptest
         parseToken (const ()) ptest (keyword "then")
         t <- exprParser opinfo (-1) ptest
         parseToken (const ()) ptest (keyword "else")
         f <- exprParser opinfo (-1) ptest
         return $ ifExpr pos $ IfBlock c t f

-- Pattern Match Parser



-- Global Parser

globalParser :: GenParser Char PState Expr
globalParser
    = let ptest = const True in
      do parseToken (const ()) ptest (return "")
         result <- exprParser [] (-10) ptest
         eofToken ptest
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

