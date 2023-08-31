{-# LANGUAGE DeriveGeneric #-}

module Model (
  Article(..),
  ArticleInfo(..)
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data Article = Article {
  info :: ArticleInfo,
  body :: Text
  } deriving (Show, Generic)

instance FromJSON Article
instance ToJSON Article

data ArticleInfo = ArticleInfo {
  path :: Text,
  title :: Text,
  update :: Text,
  description :: Text,
  tags :: [Text]
  } deriving (Show, Generic)

instance FromJSON ArticleInfo
instance ToJSON ArticleInfo
