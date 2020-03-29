module Main where

import System.Environment
import Data.List

import LispParser
import LispEvaluator

main :: IO ()
main = do
    parsed <- readExpr . head <$> getArgs
    putStrLn $ "Parsing result: " ++ (show parsed)
    putStrLn $ "Eval result: " ++ (show $ (eval parsed))