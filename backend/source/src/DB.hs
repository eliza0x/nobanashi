{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB (
  getAllArticles,
  upsertArticle,
  deleteArticle,
  getAllImages,
  storeImage,
  createStaticDirectory
  ) where

import Model
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified System.Directory as D
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy as BL
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B
import Control.Monad (unless, forM)

rootPath, imagePath, postsPath :: FilePath
rootPath = "static/"
imagePath = "static/images/"
postsPath = "static/posts/"

createStaticDirectory :: IO ()
createStaticDirectory = do
  rootExist <- D.doesDirectoryExist rootPath
  imagesExist <- D.doesDirectoryExist imagePath
  postsExist <- D.doesDirectoryExist postsPath

  unless rootExist $ D.createDirectory rootPath
  unless imagesExist $ D.createDirectory imagePath
  unless postsExist $ D.createDirectory postsPath

ls :: FilePath -> IO [FilePath]
ls = D.listDirectory

readArticle :: FilePath -> IO Article
readArticle path = do
  f <- BL.readFile $ postsPath <> path
  case A.decode f of
    Nothing -> error $ "failed to decode \"" <> path <> "\""
    Just a -> do
      -- print a
      putStrLn $ path <> "readArticle: " <> path
      return a

getAllArticles :: IO [Article]
getAllArticles = mapM readArticle =<< ls postsPath

upsertArticle :: Article -> IO ()
upsertArticle article = TL.writeFile (postsPath <> T.unpack (path article)) (A.encodeToLazyText article)

deleteArticle :: Text -> IO ()
deleteArticle path = D.removeFile $ postsPath <> T.unpack path

getAllImages :: IO [(FilePath, FilePath)]
getAllImages = do
  category <- ls imagePath
  concat <$> mapM (\c -> map (\p -> (c,p)) <$> ls (imagePath <> "/" <> c)) category

storeImage :: Text -> Text -> ByteString -> IO ()
storeImage category path image = do
  let directory = imagePath <> T.unpack category <> "/"
  exist <- D.doesDirectoryExist directory
  unless exist $ D.createDirectory directory
  B.writeFile (directory <> T.unpack path) image
