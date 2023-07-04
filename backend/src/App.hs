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

import Data.Text (Text, pack, unpack)
import Data.Map ((!?))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef
import Data.ByteString (ByteString)
import qualified Text.Pandoc as P
import Data.Maybe (fromMaybe)

parseMd :: Text -> Either Text Text
parseMd text = map' (pack . show) id $ P.runPure (P.writeHtml5String P.def =<< P.readMarkdown readerDef text)
  where
  readerDef :: P.ReaderOptions
  readerDef = P.def { P.readerExtensions = P.enableExtension P.Ext_smart P.pandocExtensions }

  map' :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  map' f _ (Left l) = Left $ f l
  map' _ g (Right r) = Right $ g r

{-
parseMd :: Text -> Either Text Article
parseMd text = do
  P.Pandoc meta blocks <- map' (pack . show) id $ P.runPure (P.readMarkdown readerDef text) 
  html <- map' (pack . show) id $ P.runPure (P.writeHtml5String P.def $ P.Pandoc meta blocks)
  let meta' = P.unMeta meta
  do
    p <- path meta'
    t <- title meta'
    u <- update meta'
    d <- description meta'
    ts <- tags meta'
    return $ Article p t u d ts (Just html)
  where
  readerDef :: P.ReaderOptions
  readerDef = P.def { P.readerExtensions = P.enableExtension P.Ext_smart P.pandocExtensions }

  map' :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  map' f _ (Left l) = Left $ f l
  map' _ g (Right r) = Right $ g r

  withErr :: err -> Maybe a -> Either err a
  withErr err Nothing = Left err
  withErr _ (Just val) = Right val
  fromMetaString :: P.MetaValue -> Maybe Text
  fromMetaString (P.MetaString t) = Just t
  fromMetaString _ = Nothing

  fromMetaList' :: [P.MetaValue] -> Maybe [Text]
  fromMetaList' [] = Just []
  fromMetaList' ((P.MetaString x):xs) = (x:) <$> fromMetaList' xs
  fromMetaList' _ = Nothing

  fromMetaList :: P.MetaValue -> Maybe [Text]
  fromMetaList (P.MetaList xs) = fromMetaList' xs
  fromMetaList _ = Nothing

  path, title, update, description :: M.Map Text P.MetaValue -> Either Text Text
  path blocks        = withErr "failed to get path"        (fromMetaString =<< blocks !? "path")
  title blocks       = withErr "failed to get blocks"      (fromMetaString =<< blocks !? "title")
  update blocks      = withErr "failed to get update"      (fromMetaString =<< blocks !? "update")
  description blocks = withErr "failed to get description" (fromMetaString =<< blocks !? "description")
  tags :: M.Map Text P.MetaValue -> Either Text [Text]
  tags blocks        = withErr "failed to get tags"        ((fromMetaList =<< blocks !? "tags") <|> (pure <$> (fromMetaString =<< blocks !? "tags")))
  -}

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
