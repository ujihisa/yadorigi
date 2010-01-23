
{-# LANGUAGE BangPatterns#-}

module Main where

import Yadorigi.Parser.DataTypes
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer
import Yadorigi.Parser.Recons

import Text.Parsec

import Control.Monad

-- Parser Tester

main :: IO ()
main = do contents <- getContents
          tokenizerResult <- return $ runParser tokenizer () "<interactive>" contents
          case tokenizerResult of
              (Right ts) ->
                  do sequence_ (map print ts)
                     parserResult <- return $ runParser globalParser () "<tokenStream>" ts
                     case parserResult of
                         (Right result) -> print result
                         (Left error) -> print error
              (Left error) -> print error
