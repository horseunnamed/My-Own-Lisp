module LispTypes where

import Data.Array
import Text.ParserCombinators.Parsec
import Control.Monad.Except

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

data LispError 
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

type ThrowsError = Either LispError

instance Show LispError where 
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwords (map show found)
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found 
    show (Parser parseErr) = "Parse error at " ++ show parseErr

trapError :: (MonadError a m, Show a) => m String -> m String 
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
