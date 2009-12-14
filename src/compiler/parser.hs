
import Text.ParserCombinators.Parsec

-- Data Types

type Position = (Int,Int)

data PState = PState Position

data Token body = Token Position body


data OperatorAssocInfo = Prefix | Infixl | Infix | Infixr

data OperatorInfo = OperatorInfo String OperatorAssocInfo Int


data ConstantValue = ConstantInt Int

data ApplyFunction = ApplyFunction Expr Expr

data Bracket = Bracket Expr

data IfBlock = IfBlock Expr Expr Expr

data Expr
    = AtomicExpr String
    | ApplyFunctionExpr ApplyFunction
    | BracketExpr Bracket
    | ListExpr [Expr]
    | IfExpr IfBlock


data PatternTree
    = DCPattern String [PatternTree] {- data constructor pattern -}
    | TuplePattern [PatternTree] {- tuple pattern -}
    | ConsPattern PatternTree PatternTree
          {- construct pattern (including list pattern) -}
    | NPlusKPattern PatternTree Int {- construct pattern -}
    | BindPattern String {- bind pattern (including wild card) -}
    | ConstantPattern ConstantValue {- constant pattern-}

-- Output Format

instance (Show body) => Show (Token body) where
    show (Token (line,column) body)
        = show line++","++show column++" "++show body

instance Show ApplyFunction where
    show (ApplyFunction func param) = "{"++show func++" "++show param++"}"

instance Show Bracket where
    show (Bracket expr) = "("++show expr++")"

instance Show IfBlock where
    show (IfBlock c t f) = "[if "++show c++" "++show t++" "++show f++"]"

instance Show Expr where
    show (AtomicExpr str) = str
    show (ApplyFunctionExpr expr) = show expr
    show (BracketExpr expr) = show expr
    show (ListExpr list) = show list
    show (IfExpr expr) = show expr

-- Parser Combinators

headPlusTail :: GenParser tok st a -> GenParser tok st a -> GenParser tok st [a]
headPlusTail head tail
    = do h <- head
         t <- many tail
         return (h:t)

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p
    = p >>= const (return x)

testPos
    :: (Show tok)
    => (Position -> Bool)
    -> Position
    -> GenParser tok st a
    -> GenParser tok st a
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

parseToken :: (String -> t)
    -> (Position -> Bool)
    -> (GenParser Char PState String)
    -> (GenParser Char PState (Token t))
parseToken conv ptest strParser
    = do (PState currentPos) <- getState
         testPos ptest currentPos
             (do token <- strParser
                 whitespace <- spacesAndComments
                 setState (PState (nextPos (token++whitespace) currentPos))
                 return (Token currentPos (conv token)))

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

nameToken :: (Position -> Bool) -> GenParser Char PState (Token String)
nameToken ptest = parseToken id ptest $ try
    (do str <- headPlusTail (letter <|> char '_') (alphaNum <|> char '_')
            <|> many1 (oneOf "!#$%&*+-./:<=>?@^")
        (if (elem str reservedWord)
            then unexpected $ show str
            else return str))

numberToken :: (Position -> Bool) -> GenParser Char PState (Token String)
numberToken ptest = parseToken id ptest $
    (try $ do string "0x"
              num <- many1 hexDigit
              return ("0x"++num))
    <|> do integer <- many1 digit
           fractional <- option "" $ headPlusTail (char '.') digit
           return (integer++fractional)

eofToken :: (Position -> Bool) -> GenParser Char PState (Token ())
eofToken ptest = parseToken (const ()) ptest $ returnConst "" eof

-- Expression Parser

exprParser
    :: [OperatorAssocInfo]
    -> Int
    -> (Position -> Bool)
    -> GenParser Char PState Expr
exprParser opinfo 12 ptest
    = bracketParser opinfo ptest
      <|> listParser opinfo ptest
      <|> (nameToken ptest >>= (\(Token _ body) -> return $ AtomicExpr body))
      <|> (numberToken ptest >>= (\(Token _ body) -> return $ AtomicExpr body))
exprParser opinfo 10 ptest
    = chainl1 (exprParser opinfo 11 ptest)
          (return (\l r -> ApplyFunctionExpr $ ApplyFunction l r))
exprParser opinfo (-1) ptest
    = ifParser opinfo ptest <|> exprParser opinfo 0 ptest
exprParser opinfo n ptest
    = exprParser opinfo (n+1) ptest

listParser
    :: [OperatorAssocInfo]
    -> (Position -> Bool)
    -> GenParser Char PState Expr
listParser opinfo ptest
    = do parseToken (const ()) ptest (string "[")
         body <- sepBy1 (exprParser opinfo (-10) ptest)
             (parseToken (const ()) ptest (string ","))
         parseToken (const ()) ptest (string "]")
         return $ ListExpr body

bracketParser
    :: [OperatorAssocInfo]
    -> (Position -> Bool)
    -> GenParser Char PState Expr
bracketParser opinfo ptest
    = do parseToken (const ()) ptest (string "(")
         body <- exprParser opinfo (-10) ptest
         parseToken (const ()) ptest (string ")")
         return $ BracketExpr $ Bracket body

ifParser
    :: [OperatorAssocInfo]
    -> (Position -> Bool)
    -> GenParser Char PState Expr
ifParser opinfo ptest
    = do parseToken (const ()) ptest (keyword "if")
         c <- exprParser opinfo (-1) ptest
         parseToken (const ()) ptest (keyword "then")
         t <- exprParser opinfo (-1) ptest
         parseToken (const ()) ptest (keyword "else")
         f <- exprParser opinfo (-1) ptest
         return $ IfExpr $ IfBlock c t f

-- Pattern Match Parser



-- Global Parser

globalParser :: GenParser Char PState Expr
globalParser
    = let ptest = const True in
      do parseToken (const ()) ptest (return "")
         result <- exprParser [] (-10) ptest
         eof
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

