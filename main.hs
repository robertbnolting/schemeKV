module Main where
import Data.Hashable
import qualified Data.HashTable.IO as H
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Except
import Numeric
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

type HashTable k v = H.BasicHashTable k v

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable"
                               var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                  ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | HashMap (HashTable LispVal LispVal)

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
showVal (HashMap _) = "<HashMap>"

instance Show LispVal where show = showVal

instance Eq LispVal where
  Atom x == Atom y = x == y
  String x == String y = x == y
  Number x == Number y = x == y
  Character x == Character y = x == y

instance Hashable LispVal where
  hashWithSalt s (Atom val) = s + (hash val)
  hashWithSalt s (String val) = s + (hash val)
  hashWithSalt s (Number val) = s + (hash val)
  hashWithSalt s (Character val) = s + (hash val)

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

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

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

makeHashMap :: Env -> IO LispVal
makeHashMap env = do
  ht <- H.new
  return $ HashMap ht

hashmapInsert :: Env -> LispVal -> LispVal -> LispVal -> IO ()
hashmapInsert env (HashMap ht) key value = H.insert ht key value

hashmapDelete :: Env -> LispVal -> LispVal -> IO ()
hashmapDelete env (HashMap ht) key = H.delete ht key

hashmapLookup :: Env -> LispVal -> LispVal -> IO LispVal 
hashmapLookup env (HashMap ht) key = do
  res <- H.lookup ht key
  case res of
    Nothing  -> return $ Bool False
    Just val -> return val

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs
       >>= evalBody

  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Character _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      otherwise  -> eval env conseq

eval env (List [Atom "if", pred, conseq]) =
  do
    result <- eval env pred
    case result of
      Bool False -> return $ Bool False
      otherwise  -> eval env conseq

eval env (List (Atom "cond" : clauses)) = eval_cond clauses
  where
    eval_cond ((List [test]) : rest) = do
      result <- eval env test
      case result of
        Bool False -> eval_cond rest
        otherwise  -> return $ Bool True

    eval_cond ((List (test : exprs)) : rest) = do
      result <- eval env test

      case result of
        Bool False -> eval_cond rest
        otherwise  -> mapM (eval env) exprs >>= return . last

eval env (List (Atom "case" : key : clauses)) = do
  evaled_key <- eval env key
  eval_case evaled_key clauses
  where
    eval_case k ((List (Atom "else" : exprs)) : rest) = mapM (eval env) exprs >>=
      return . last

    eval_case k ((List ((List (t:tests)) : exprs)) : rest) = do
      result <- liftThrows $ eqv [k, t]
      case result of
        Bool False -> if null tests
                        then eval_case k rest
                        else eval_case k (List ((List tests) : exprs) : rest)
        otherwise  -> mapM (eval env) exprs >>= return . last

eval env (Atom "else") = return $ Bool True

eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

eval env (Atom id) = getVar env id

eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body

eval env (List [Atom "hashmap-make", Atom var]) = do
  hm <- liftIO $ makeHashMap env
  defineVar env var hm

eval env (List [Atom "hashmap-insert", Atom var, (List [key, value])]) = do
  v <- getVar env var
  liftIO $ hashmapInsert env v key value
  return value

eval env (List [Atom "hashmap-delete", Atom var, key]) = do
  v <- getVar env var
  liftIO $ hashmapDelete env v key
  return key

eval env (List [Atom "hashmap-lookup", Atom var, key]) = do
  v <- getVar env var
  val <- liftIO $ hashmapLookup env v key
  return val

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env "" = return ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runFile :: [String] -> IO ()
runFile args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runFile $ args
