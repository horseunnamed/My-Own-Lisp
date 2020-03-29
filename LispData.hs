module LispData where

import Data.Array

data LispVal 
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    | Number LispNumber
    | String String
    | Bool Bool
    | Char Char
    deriving Show

data LispNumber 
    = Int Integer
    | Real Double
    deriving Show
