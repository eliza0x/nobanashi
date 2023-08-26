module Storage where

import Model
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import qualified Storage.GCP as SG

getArticle :: Text -> IO Article
getArticle path = do
  f <- SG.getArticle path :: IO ByteString
  BC.putStrLn f
  case A.decode $ B.fromStrict f of
    Nothing -> error $ "failed to decode \"" <> T.unpack path <> "\""
    Just a -> return a

getInfos :: IO [Article]
getInfos = do
  sitemap <- A.decode . B.fromStrict <$> SG.getSitemap
  case sitemap of
    Just s -> return s
    Nothing -> error "sitemapが壊れています"

uploadArticle :: Article -> IO ()
uploadArticle article = do
  infos <- getInfos
  let infos' = TL.toStrict . A.encodeToLazyText . upsert article $ infos
  let a = TL.toStrict $ A.encodeToLazyText article :: Text
      p = path article :: Text
  SG.uploadSitemap infos'
  SG.uploadArticle p a

upsert :: Article -> [Article] -> [Article]
upsert a as = map snd . M.toList
  . M.insert (path a) (dropBody a) 
  . M.fromList $ map (\x -> (path x, x)) $ as