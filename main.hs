module Main where

import System.Environment
import Data.List

import LispParser
import LispEvaluator
import LispTypes

main :: IO ()
main = do
    parsed <- readExpr . head <$> getArgs
    let evaled = show <$> (parsed >>= eval)
    putStrLn $ extractValue $ trapError evaled