module LispParser where 

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Numeric
import Data.Array
import Control.Monad.Except

import LispTypes

parseSymbol :: P.Parser Char
parseSymbol = P.oneOf "!$%&|*+-/:<=>?@^_~"

parseSpaces :: P.Parser ()
parseSpaces = P.skipMany1 P.space

escapedChar :: P.Parser Char
escapedChar = P.char '\\' >> P.oneOf "\\\"trn"

parseString :: P.Parser LispVal
parseString = do
    P.char '"'
    x <- P.many $ P.noneOf "\"\\" <|> escapedChar
    P.char '"'
    return $ String x

parseAtom :: P.Parser LispVal
parseAtom = do
    first <- P.letter <|> parseSymbol
    rest <- P.many (P.letter <|> P.digit <|> parseSymbol)
    return $ Atom $ first:rest

parseBool :: P.Parser LispVal
parseBool = do
    P.char '#'
    x <- P.oneOf "ft"
    return $ case x of
        'f' -> Bool True
        't' -> Bool False

parseNumber :: P.Parser LispVal
parseNumber = Number <$> (P.try parseFloat <|> P.try parseInt)

parseInt :: P.Parser LispNumber
parseInt =
    P.try parseDecimal1
    <|> P.try parseDecimal2
    <|> P.try parseHex
    <|> P.try parseOct
    <|> P.try parseBin

parseDecimal1 :: P.Parser LispNumber
parseDecimal1 = Int . read <$> P.many1 P.digit

parseDecimal2 :: P.Parser LispNumber
parseDecimal2 = Int . read <$> (P.string "#d" >> P.many1 P.digit)

parseOct :: P.Parser LispNumber
parseOct = Int . oct2dig <$> (P.string "#o" >> P.many1 P.octDigit)

parseHex :: P.Parser LispNumber
parseHex = Int . hex2dig <$> (P.string "#x" >> P.many1 P.hexDigit)

parseBin :: P.Parser LispNumber
parseBin = Int . bin2dig <$> (P.string "#b" >> P.many1 (P.oneOf "01"))

parseFloat :: P.Parser LispNumber
parseFloat = do
    x <- P.many1 P.digit
    P.char '.'
    y <- P.many1 P.digit
    return $ Real $ fst.head $ readFloat (x ++ "." ++ y)

parseChar :: P.Parser LispVal
parseChar =
    P.try parseNewLineChar
    <|> P.try parseSpace
    <|> P.try parseSimpleChar

parseNewLineChar :: P.Parser LispVal
parseNewLineChar = Char . const '\n' <$> P.string "#\\newline"

parseSpace :: P.Parser LispVal
parseSpace = Char . const ' ' <$> P.string "#\\space"

parseSimpleChar :: P.Parser LispVal
parseSimpleChar = do
    P.try $ P.string "#\\"
    value <- P.try P.anyChar
    P.notFollowedBy P.alphaNum
    return $ Char value

oct2dig x = fst $ head $ readOct x
hex2dig x = fst $ head $ readHex x
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
    let old = 2 * digint + (if x == '0' then 0 else 1)
    in bin2dig' old xs

parseList :: P.Parser LispVal
parseList = List <$> P.sepBy parseExpr parseSpaces

parseDottedList :: P.Parser LispVal
parseDottedList = do
    head <- P.endBy parseExpr parseSpaces
    tail <- P.char '.' >> parseSpaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: P.Parser LispVal
parseQuoted = do
    P.char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseAnyList :: P.Parser LispVal
parseAnyList = do
    P.char '('
    x <- P.try parseList <|> parseDottedList
    P.char ')'
    return x

parseVector :: P.Parser LispVal
parseVector = do
    P.string "#("
    arrayValues <- P.sepBy parseExpr parseSpaces
    P.char ')'
    return $ Vector $ listArray (0, length arrayValues - 1) arrayValues

parseExpr :: P.Parser LispVal
parseExpr =
    parseAtom
    <|> parseString
    <|> P.try parseNumber
    <|> P.try parseBool
    <|> P.try parseChar
    <|> parseQuoted
    <|> parseAnyList
    <|> parseVector

readExpr :: String -> ThrowsError LispVal
readExpr input = case P.parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
