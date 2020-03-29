module Main where

import System.Environment
import Data.List

import LispParser

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
