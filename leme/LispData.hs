module LispData where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)

type Env = IORef [(String, IORef LispVal)]
type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected
                                            ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                            ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
