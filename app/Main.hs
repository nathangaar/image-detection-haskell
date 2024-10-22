{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Lib
import System.Environment (lookupEnv)
import Web.Scotty (ActionM, get, param, rescue, scotty)

getPGConfig :: IO ConnectInfo
getPGConfig = do
  host <- lookupEnvOrDefault "DATABASE_HOST" "0.0.0.0"
  dbname <- lookupEnvOrDefault "DATABASE_NAME" "images"
  user <- lookupEnvOrDefault "DATABASE_USER" "image_user"
  password <- lookupEnvOrDefault "DATABASE_PASSWORD" "Password123"
  pure
    defaultConnectInfo
      { connectHost = host,
        connectDatabase = dbname,
        connectUser = user,
        connectPassword = password
      }

lookupEnvOrDefault :: String -> String -> IO String
lookupEnvOrDefault var def = do
  maybeVal <- lookupEnv var
  return $ maybe def id maybeVal

main :: IO ()
main = do
  conn <- getPGConfig >>= connect
  routes conn

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/images" $ do
    objectsParam <- param "objects" `rescue` handleError
    let objectsList = if T.null objectsParam then [] else T.splitOn "," objectsParam
    getImages conn objectsList

  get "/images/:id" $ getImage conn

handleError :: SomeException -> ActionM T.Text
handleError _ = return ""
