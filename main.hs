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
showVal (Character contents) = [contents]
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
  x <- many (noneOf " )")
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

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

typeOp :: String -> [LispVal] -> ThrowsError LispVal
typeOp "symbol" [Atom _]   = return (Bool True)
typeOp "symbol" [t]        = return (Bool False)
typeOp "string" [String _] = return (Bool True)
typeOp "string" [t]        = return (Bool False)
typeOp "number" [Number _] = return (Bool True)
typeOp "number" [t]        = return (Bool False)
typeOp _ badArgList        = throwError $ NumArgs 1 badArgList

symbolOp :: String -> [LispVal] -> ThrowsError LispVal
symbolOp "toStr" [Atom sym]   = return (String sym)
symbolOp "toStr" [t]          = throwError $ TypeMismatch "symbol" t
symbolOp "fromStr" [String s] = return (Atom s)
symbolOp "fromStr" [t]        = throwError $ TypeMismatch "string" t
symbolOp _ badArgList         = throwError $ NumArgs 1 badArgList

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left <- unpacker $ args !! 0
                                       right <- unpacker $ args !! 1
                                       return $ Bool $ left `op` right

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]
  = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

toString :: [LispVal] -> ThrowsError LispVal
toString s = liftM String $ appendStr s
  where appendStr (Character c : cs) = do str <- appendStr cs
                                          return $ (c : str)
        appendStr [] = return ""
        appendStr (notChar:_) = throwError $ TypeMismatch "char" notChar
        appendStr :: [LispVal] -> ThrowsError String

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return . Number . toInteger $ length s
stringLength [t]        = throwError $ TypeMismatch "string" t
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number n] = return . Character $ s !! (fromIntegral n)
stringRef [t, Number n]               = throwError $ TypeMismatch "string" t
stringRef [String s, t]               = throwError $ TypeMismatch "number" t
stringRef badArgList           = throwError $ NumArgs 2 badArgList

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
              ("string->symbol", symbolOp "fromStr"),
			  ("=", numBoolBinop (==)),
			  ("<", numBoolBinop (<)),
			  (">", numBoolBinop (>)),
			  ("/=", numBoolBinop (/=)),
			  (">=", numBoolBinop (>=)),
			  ("<=", numBoolBinop (<=)),
			  ("&&", boolBoolBinop (&&)),
			  ("||", boolBoolBinop (||)),
			  ("string=?", strBoolBinop (==)),
			  ("string<?", strBoolBinop (<)),
			  ("string>?", strBoolBinop (>)),
			  ("string<=?", strBoolBinop (<=)),
			  ("string>=?", strBoolBinop (>=)),
			  ("car", car),
			  ("cdr", cdr),
			  ("cons", cons),
			  ("eq?", eqv),
			  ("eqv?", eqv),
			  ("string", toString),
			  ("string-length", stringLength),
			  ("string-ref", stringRef)]

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
eval (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval pred
    case result of
      Bool False -> eval alt
      otherwise  -> eval conseq

eval (List (Atom "cond" : clauses)) = eval_cond clauses
  where
    eval_cond ((List [test, expr]) : rest) = do
      result <- eval test
      case result of
        Bool False -> eval_cond rest
        otherwise  -> eval expr
    eval_cond ((List [test]) : rest) = do
      result <- eval test
      case result of
        Bool False -> eval_cond rest
        otherwise  -> return $ Bool True

eval (List (Atom "case" : key : clauses)) = do
  evaled_key <- eval key
  eval_case evaled_key clauses
  where
    eval_case k ((List [Atom "else", expr]) : rest) = eval expr

    eval_case k ((List [List (t:tests), expr]) : rest) = do
      result <- eqv [k, t]
      case result of
        Bool False -> if null tests
                        then eval_case k rest
                        else eval_case k (List [List tests, expr] : rest)
        otherwise  -> eval expr

eval (Atom "else") = return $ Bool True
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
