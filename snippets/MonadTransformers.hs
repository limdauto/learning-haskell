{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
    deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
        [_, domain] -> Right domain
        _           -> Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult res =
    T.putStrLn $ case res of
        Right token -> append "Logged in with token: " token
        Left NoSuchUser -> "User not found"
        Left WrongPassword -> "Wrong password"
        Left InvalidEmail -> "Invalid email"


getToken :: ExceptIO LoginError Text
getToken = do
    liftIO $ T.putStrLn "Enter your email address: "
    input <- liftIO T.getLine
    liftEither $ getDomain input

users :: Map Text Text
users = Map.fromList[("localhost", "password"), ("example.com", "qwerty1234")]

data ExceptIO e a = ExceptIO {
    runExceptIO :: IO (Either e a)
}

instance Functor (ExceptIO a) where
    fmap f = ExceptIO . fmap (fmap f) . runExceptIO

instance Applicative (ExceptIO a) where
    pure = ExceptIO . return . Right
    f <*> x = ExceptIO $ liftA2 (<*>) (runExceptIO f) (runExceptIO x)

instance Monad (ExceptIO a) where
    return = pure
    x >>= f = ExceptIO $ (runExceptIO x) >>= either (return . Left) (runExceptIO . f)

liftEither :: Either e a -> ExceptIO e a
liftEither x = ExceptIO (return x)

liftIO :: IO a -> ExceptIO e a
liftIO x = ExceptIO (fmap Right x)

userLogin :: ExceptIO LoginError Text
userLogin = do
    token <- getToken
    userpw <- maybe (throwE NoSuchUser) return (Map.lookup token users)
    password <- liftIO (T.putStrLn "Enter your password: " >> T.getLine)
    if userpw == password 
        then return token
        else throwE WrongPassword

throwE :: e -> ExceptIO e a
throwE err = liftEither (Left err)

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler = ExceptIO $ do
    result <- runExceptIO throwing
    case result of
        Left err -> runExceptIO (handler err)
        success -> return success

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err