module Main where

import System.IO
import Data.Functor
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readHex, readOct)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default string)              = "Default: " ++ string

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

interpretEscapedChar :: Parser Char
interpretEscapedChar = do
  x <- oneOf "\"\\nrt"
  return $ case x of
    't' -> '\t'
    'r' -> '\r'
    'n' -> '\n'
    _ -> x

escapeStringChar :: Parser Char
escapeStringChar = noneOf ['\\', '"'] <|> (char '\\' >> interpretEscapedChar)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many escapeStringChar
  char '"'
  return (String x)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

noPrefixNumber :: Parser LispVal
noPrefixNumber = Number . read <$> many1 digit

withPrefixNumber :: Parser LispVal
withPrefixNumber = do
  char '#'
  prefix <- oneOf "dox"
  case prefix of
    'd' -> noPrefixNumber
    'o' -> Number . read . show . fst . head . readOct <$> many1 (oneOf "01234567")
    'x' -> Number . read . show . fst . head . readHex <$> many1 (digit <|> oneOf "abcdef")

parseNumber :: Parser LispVal
parseNumber = noPrefixNumber <|> withPrefixNumber

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseAtom
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params <&> Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


-- There is probably a way to deduplicate this
symbolCheck :: [LispVal] -> ThrowsError LispVal
symbolCheck [] = return $ Bool False
symbolCheck [Atom _] = return $ Bool True
symbolCheck ((Atom _):vs) = symbolCheck vs
symbolCheck _ = return $ Bool False

stringCheck :: [LispVal] -> ThrowsError LispVal
stringCheck [] = return $ Bool False
stringCheck [String _] = return $ Bool True
stringCheck ((String _):vs) = stringCheck vs
stringCheck _ = return $ Bool False

numberCheck :: [LispVal] -> ThrowsError LispVal
numberCheck [] = return $ Bool False
numberCheck [Number _] = return $ Bool True
numberCheck ((Number _):vs) = numberCheck vs
numberCheck _ = return $ Bool False

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", symbolCheck),
              ("string?", stringCheck),
              ("number?", numberCheck)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

main :: IO ()
main = do
     args <- getArgs
     let evaled = fmap show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled
