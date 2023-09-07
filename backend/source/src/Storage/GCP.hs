{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.GCP where

import Control.Lens           ((&), (.~), (<&>), (?~))
import qualified Network.Google as G
import qualified Network.Google.Storage as Storage
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit ( (.|), runConduit )
import qualified Conduit as C
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource (liftResourceT)
import Data.Text (Text)
import qualified Data.String as S
import Network.HTTP.Media.MediaType ((//))

bucket_articles, bucket_info, file_sitemap :: Text
bucket_articles = "nobanashi-articles"
bucket_info = "nobanashi-info"
file_sitemap = "sitemap.json"

genEnv = do
    -- lgr  <- G.newLogger G.Debug stdout
    -- G.newEnv <&> (G.envLogger .~ lgr) . (G.envScopes .~ Storage.storageReadWriteScope)
    G.newEnv <&> (G.envScopes .~ Storage.storageReadWriteScope)

upload :: Text -> Text -> String -> IO ()
upload bkt key body = do
    env <- genEnv
    let ins = Storage.objectsInsert bkt Storage.object' & Storage.oiName ?~ key
        obj = S.fromString body & G.bodyContentType .~ ("application" // "json")
    _ <- C.runResourceT $ G.runGoogle env (G.upload ins obj)
    return ()

uploadSitemap :: String -> IO ()
uploadSitemap = upload bucket_info file_sitemap

uploadArticle :: Text -> String -> IO ()
uploadArticle = upload bucket_articles

-- 例外が飛ぶ、なんかいい感じのハンドリングを考える
get :: Text -> Text -> IO ByteString
get bkt file = do
    env <- genEnv
    ret <- C.runResourceT . G.runGoogle env $ do
        stream <- G.download $ Storage.objectsGet bkt file
        liftResourceT $ runConduit (stream .| CC.sinkList)
    return $ B.concat ret

getSitemap :: IO ByteString
getSitemap = get bucket_info file_sitemap

getArticle :: Text -> IO ByteString
getArticle article_id = get bucket_articles article_id
