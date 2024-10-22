{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getImage,
    getImages,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.HTTP.Types.Status (status200, status400, status500)
import Web.Scotty (ActionM, param, status)
import qualified Web.Scotty as S

data Image = Image
  { idImage :: UUID,
    metaData :: Value,
    imageUrl :: String,
    label :: String
  }

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field <*> field

instance ToJSON Image where
  toJSON (Image idImage metaData imageUrl label) =
    object
      [ "id" .= idImage,
        "meta_data" .= metaData,
        "image_url" .= imageUrl,
        "label" .= label
      ]

instance S.Parsable UUID where
  parseParam t = case UUID.fromString (T.unpack $ TL.toStrict t) of
    Just uuid -> Right uuid
    Nothing -> Left "Invalid UUID"

getImages :: Connection -> [T.Text] -> ActionM ()
getImages conn objectsList = do
  liftIO $ print objectsList
  result <- liftIO $ try (query_ conn "SELECT uuid, meta_data, image_url, label FROM images") :: ActionM (Either SomeException [Image])
  case result of
    Left err -> do
      status status500
      S.json $ object ["error" .= ("Failed to fetch images" :: String), "details" .= show err]
    Right images -> do
      status status200
      S.json $ object ["images" .= images]

getImage :: Connection -> ActionM ()
getImage conn = do
  _idImage <- param "id" :: ActionM UUID
  result <- liftIO $ try (query conn "SELECT uuid, meta_data, image_url, label FROM images WHERE uuid = ?" (Only _idImage)) :: ActionM (Either SomeException [Image])
  case result of
    Left err -> do
      status status500
      S.json $ object ["error" .= ("Failed to fetch image" :: String), "details" .= show err]
    Right [] -> do
      status status400
      S.json $ object ["error" .= ("Image not found" :: String)]
    Right (image : _) -> do
      status status200
      S.json image
