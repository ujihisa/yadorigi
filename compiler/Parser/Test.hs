
module Main where

import Yadorigi.Parser
import Yadorigi.Parser.DataTypes
import Yadorigi.Parser.Recons
import Text.ParserCombinators.Parsec

-- Parser Tester

strParser :: String -> Either ParseError Expr
strParser str = runParser globalParser () "<contents>" str

printEither :: (Show a,Show b) => Either a b -> IO ()
printEither (Right a) = print a
printEither (Left b) = print b

main :: IO ()
main = do contents <- getContents
          printEither $ strParser contents

