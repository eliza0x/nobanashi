module Storage where

import Model
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.ByteString ( ByteString, fromStrict )
import qualified Data.ByteString.Char8 as BC
import Control.Monad (unless, forM)
import qualified Data.Map as M

import qualified Storage.GCP as SG

getArticle :: Text -> IO Article
getArticle path = do
  f <- SG.getArticle path :: IO ByteString
  BC.putStrLn f
  case A.decode $ fromStrict f of
    Nothing -> error $ "failed to decode \"" <> T.unpack path <> "\""
    Just a -> return a

getInfos :: IO [Article]
getInfos = do
  sitemap <- A.decode . fromStrict <$> SG.getSitemap
  case sitemap of
    Just s -> return s
    Nothing -> error "sitemapが壊れています"

uploadArticle :: Article -> IO ()
uploadArticle article = do
  infos <- getInfos
  let infos' = TL.toStrict . A.encodeToLazyText . M.toList
             . M.insert (path article) (dropBody article) 
             . M.fromList $ map (\i -> (path i, i)) $ infos
  let a = TL.toStrict $ A.encodeToLazyText article :: Text
      p = path article :: Text
  SG.uploadSitemap infos'
  SG.uploadArticle p a
