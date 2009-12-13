
import Text.ParserCombinators.Parsec

-- Data Types

type Position = (Int,Int)

data PState = PState Position

data Token body = Token Position body String


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
    show (ApplyFunction func param) = "["++show func++" "++show param++"]"

instance Show Bracket where
    show (Bracket expr) = "("++show expr++")"

instance Show Expr where
    show (AtomicExpr str) = str
    show (ApplyFunctionExpr expr) = show expr
    show (BracketExpr expr) = show expr

-- Common Parser Combinators

headPlusTail :: GenParser tok st a -> GenParser tok st a -> GenParser tok st [a]
headPlusTail head tail
    = do h <- head
         t <- many tail
         return (h:t)

returnConst :: b -> GenParser tok st a -> GenParser tok st b
returnConst x p
    = p >>= const (return x)

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
parseToken conv postest strParser
    = do (PState currentPos) <- getState
         if postest currentPos
             then (do token <- strParser
                      whitespace <- spacesAndComments
                      setState (PState (nextPos (token++whitespace) currentPos))
                      return (Token currentPos (conv token)))
             else (do eof >>= const (unexpected "end of file")
                      unexpected "token position")

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
nameToken postest = parseToken id postest
    (do str <- headPlusTail (letter <|> char '_') (alphaNum <|> char '_')
            <|> many1 (oneOf "!#$%&*+-./:<=>?@^")
        (if (elem str reservedWord)
            then unexpected "reserved word"
            else return str))

numberToken :: (Position -> Bool) -> GenParser Char PState (Token String)
numberToken postest = parseToken id postest
    (do integer <- many1 digit
        fractional <- option "" $ headPlusTail (char '.') digit
        return (integer++fractional))

eofToken :: (Position -> Bool) -> GenParser Char PState (Token ())
eofToken postest = parseToken (const ()) postest $ returnConst "" eof

-- Parser

exprParser
    :: [OperatorAssocInfo]
    -> Int
    -> (Position -> Bool)
    -> GenParser Char PState Expr
exprParser opinfo 12 postest
    = bracketParser opinfo postest
      <|> (nameToken postest>>=(\(Token _ body) -> return $ AtomicExpr body))
      <|> (numberToken postest>>=(\(Token _ body) -> return $ AtomicExpr body))
exprParser opinfo 10 postest
    = chainl1 (exprParser opinfo 11 postest)
          (return (\l r -> ApplyFunctionExpr $ ApplyFunction l r))
exprParser opinfo n postest
    = exprParser opinfo (n+1) postest

bracketParser
    :: [OperatorAssocInfo]
    -> (Position -> Bool)
    -> GenParser Char PState Expr
bracketParser opinfo postest
    = do (PState (xpos,ypos)) <- getState
         newPostest <- return (\pos@(x,y) -> postest pos&&ypos <= y)
         parseToken (const ()) newPostest (string "(")
         body <- exprParser opinfo 0 newPostest
         parseToken (const ()) newPostest (string ")")
         return $ BracketExpr $ Bracket body

{-
ifParser :: (Position -> Bool) -> GenParser Char PState If
ifParser postest
    = do parseToken (const ()) newPostest (string "if")
         
         parseToken (const ()) newPostest (string "then")
         
         parseToken (const ()) newPostest (string "else")
-}

-- Global Parser

globalParser :: GenParser Char PState Expr
globalParser
    = let postest = const True in
      do parseToken [] (const ()) postest $ return ""
         exprParser [] 0 postest

-- Parser Tester

strParser :: String -> IO ()
strParser str
    = case runParser globalParser (PState (0,0)) "<string>" str of
        Left err -> print err
        Right result -> print result

interactiveParser :: IO ()
interactiveParser
    = do input <- getInput
         putStr input
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

