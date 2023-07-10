{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (
  parseMd,
  newCache,
  getAllArticleHeaders,
  getAllArticles,
  getArticle,
  upsertArticle,
  deleteArticle,
  getTags,
  getAllImages,
  getCategory,
  getImages,
  storeImage
  ) where

import Model
import qualified DB
import qualified Parser

import Data.Text (Text, pack, unpack)
import Data.Map ((!?))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

parseMd = Parser.parse

newCache :: IO Cache
newCache = do
  as <- DB.getAllArticles
  is <- DB.getAllImages
  let article_cache = M.fromList $ zip (map path as) as
      image_cache = foldr (\(k,v) d -> M.insert k (v : fromMaybe [] (d !? k)) d) M.empty is
  return $ Cache article_cache image_cache

-- body抜きのArticle
getAllArticleHeaders :: IORef Cache -> IO [Article]
getAllArticleHeaders cache = do
  c <- article_cache <$> readIORef cache
  return . map (\(Article p t u d ts _) -> Article p t u d ts Nothing) $ M.elems c

getAllArticles :: IORef Cache -> IO [Article]
getAllArticles cache = M.elems . article_cache <$> readIORef cache

getArticle :: IORef Cache -> Text -> IO (Maybe Article)
getArticle cache path = do
  c <- article_cache <$> readIORef cache
  case c !? path of
    Just article -> return $ Just article
    Nothing -> do
      error ("failed to get article: \"" <> unpack path <> "\"" :: String)

upsertArticle :: IORef Cache -> Article -> IO ()
upsertArticle cache article = do
  DB.upsertArticle article
  writeIORef cache =<< newCache

deleteArticle :: IORef Cache -> Text -> IO ()
deleteArticle cache path = do
  DB.deleteArticle path
  writeIORef cache =<< newCache

getTags :: IORef Cache -> IO [Text]
getTags cache = do
  c <- article_cache <$> readIORef cache
  return $ S.toList . S.fromList . concatMap tags $ M.elems c

getAllImages :: IORef Cache -> IO [FilePath]
getAllImages cache = do
  c <- image_cache <$> readIORef cache
  return . concatMap (\(c, ps) -> map (\p -> c <> "/" <> p) ps) $ M.toList c

getCategory :: IORef Cache -> IO [FilePath]
getCategory cache = do
  c <- image_cache <$> readIORef cache 
  return $ M.keys c

getImages :: IORef Cache -> FilePath -> IO [FilePath]
getImages cache category = do
  c <- image_cache <$> readIORef cache 
  return . fromMaybe [] $ c !? category

storeImage :: IORef Cache -> Text -> Text -> ByteString -> IO ()
storeImage cache category path image = do
  DB.storeImage category path image
  writeIORef cache =<< newCache
