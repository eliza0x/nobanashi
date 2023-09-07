module Storage (
  getInfos,
  getArticle,
  uploadArticle
  ) where

import Model
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map as M

import qualified Storage.GCP as SG

getInfos :: IO [ArticleInfo]
getInfos = do
  sitemap <- A.eitherDecode . B.fromStrict <$> SG.getSitemap
  case sitemap of
    Right s -> return s
    Left e -> error $ "sitemapが壊れています: " ++ e

uploadInfos :: [ArticleInfo] -> IO ()
uploadInfos = SG.uploadSitemap . BLC.unpack . A.encode

appendInfo :: ArticleInfo -> IO ()
appendInfo i = uploadInfos . upsert i =<< getInfos

getArticle :: Text -> IO Article
getArticle article_path = do
  f <- SG.getArticle article_path :: IO ByteString
  case A.eitherDecode $ B.fromStrict f of
    Left e -> error $ "failed to decode \"" <> T.unpack article_path <> "\": " <> e
    Right a -> return a

uploadArticle :: Article -> IO ()
uploadArticle a = do
  print a
  let json = BLC.unpack $ A.encode a :: String
  appendInfo $ info a -- 記事更新に合わせてsitemapも更新
  SG.uploadArticle (path $ info a) json

upsert :: ArticleInfo -> [ArticleInfo] -> [ArticleInfo]
upsert a as = map snd . M.toList
  . M.insert (path a) a
  . M.fromList $ map (\x -> (path x, x)) as