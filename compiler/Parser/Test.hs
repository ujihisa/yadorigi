
module Main where

import Yadorigi.Parser
import Yadorigi.Parser.DataTypes
import Text.ParserCombinators.Parsec

-- Parser Tester

strParser :: String -> IO ()
strParser str =
    case runParser globalParser () "<contents>" str of
         Left err -> print err
         Right result -> print result

main :: IO ()
main = do contents <- getContents
          strParser contents

