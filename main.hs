module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ [contents]
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal


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

instance Show LispError where show = showError


type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  x <- many (noneOf " ")
  return $ case x of
    "space" -> Character ' '
    "newline" -> Character '\n'
    "return" -> Character '\r'
    [c] -> Character c

parseEscaped :: Parser Char
parseEscaped = do
  char '\\'
  c <- oneOf "\\\"nrt"
  return $ case c of
	'"'  -> c
	'\\' -> c
	'n'  -> '\n'
	'r'  -> '\r'
	't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseEscaped <|> noneOf "\"\\")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseHex :: Parser String
parseHex = do
  char '#'
  char 'x'
  x <- many1 hexDigit
  return x

parseOct :: Parser String
parseOct = do
  char '#'
  char 'o'
  x <- many1 octDigit
  return x

parseBin :: Parser String
parseBin = do
  char '#'
  char 'b'
  x <- many1 (oneOf "01")
  return x

parseDec :: Parser String
parseDec = do
  char '#'
  char 'd'
  x <- many1 digit
  return x

parseNumber :: Parser LispVal
parseNumber = do
  num <- (parseHex <|> parseOct <|> parseBin <|> parseDec <|> many1 digit)
  return $ Number $ read num

parseFloat :: Parser LispVal
parseFloat = do
  pre <- many1 digit
  char '.'
  post <- many1 digit
  return $ Float $ ((read (pre ++ "." ++ post)) :: Float)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

parseBackquoted :: Parser LispVal
parseBackquoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
		 <|> parseAtom
         <|> parseString
		 <|> try parseFloat
		 <|> try parseNumber
		 <|> parseQuoted
		 <|> parseBackquoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

typeOp :: String -> [LispVal] -> ThrowsError LispVal
typeOp "symbol" [Atom _]   = return (Bool True)
typeOp "symbol" [t]        = return (Bool False)
typeOp "string" [String _] = return (Bool True)
typeOp "string" [t]        = return (Bool False)
typeOp "number" [Number _] = return (Bool True)
typeOp "number" [t]        = return (Bool False)

symbolOp :: String -> [LispVal] -> ThrowsError LispVal
symbolOp "toStr" [Atom sym]   = return (String sym)
symbolOp "toStr" [t]          = throwError $ TypeMismatch "symbol" t
symbolOp "fromStr" [String s] = return (Atom s)
symbolOp "fromStr" [t]        = throwError $ TypeMismatch "string" t

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", typeOp "symbol"),
              ("string?", typeOp "string"),
              ("number?", typeOp "number"),
              ("symbol->string", symbolOp "toStr"),
              ("string->symbol", symbolOp "fromStr")]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitve function args" func)
  ($ args) $ lookup func primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
