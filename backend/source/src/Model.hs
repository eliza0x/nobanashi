{-# LANGUAGE DeriveGeneric #-}

module Model (
  Article(..),
  updateBody,
  Cache(..)
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Map.Strict
import Data.ByteString (ByteString)

data Article = Article {
  path :: Text,
  title :: Text,
  update :: Text,
  description :: Text,
  tags :: [Text],
  body :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Article
instance ToJSON Article

updateBody :: Article -> Maybe Text -> Article
updateBody (Article p t u d ts _) b = Article p t u d ts b

data Cache = Cache {
  article_cache :: Map Text Article,
  image_cache :: Map FilePath [FilePath]
} deriving (Show)