{-# LANGUAGE DeriveGeneric #-}

module Model (
  Article(..),
  dropBody,
  updateBody
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
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

dropBody :: Article -> Article
dropBody (Article p t u d ts b) = Article p t u d ts Nothing

updateBody :: Article -> (Maybe Text) -> Article
updateBody (Article p t u d ts b) b' = Article p t u d ts b'

