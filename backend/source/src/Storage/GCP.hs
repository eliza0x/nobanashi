{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.GCP where

import Control.Lens           ((&), (.~), (<&>), (?~), (^.))
import qualified Control.Lens as L
import qualified Network.Google as G
import           Network.Google.Types   (GBody (..))
import qualified Network.Google.Storage as Storage
import qualified Network.Google.Storage.Types as ST
import System.IO              (stdout)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource (ResourceT, liftResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.String as S

bucket_articles = "nobanashi-articles"
bucket_info = "nobanashi-info"
file_sitemap = "sitemap.json"

genEnv = do
    -- lgr  <- G.newLogger G.Debug stdout
    -- G.newEnv <&> (G.envLogger .~ lgr) . (G.envScopes .~ Storage.storageReadWriteScope)
    G.newEnv <&> (G.envScopes .~ Storage.storageReadWriteScope)

upload :: Text -> Text -> Text -> IO ()
upload bkt key body = do
    env <- genEnv
    C.runResourceT . G.runGoogle env $ do
        G.upload (Storage.objectsInsert bkt Storage.object' & Storage.oiName ?~ key) (S.fromString $ T.unpack body)
    return ()

uploadSitemap :: Text -> IO ()
uploadSitemap = upload bucket_info file_sitemap

uploadArticle :: Text -> Text -> IO ()
uploadArticle = upload bucket_articles

-- 例外が飛ぶ、なんかいい感じのハンドリングを考える
get :: Text -> Text -> IO ByteString
get bkt file = do
    env <- genEnv
    ret <- C.runResourceT . G.runGoogle env $ do
        stream <- G.download $ Storage.objectsGet bkt file
        liftResourceT $ runConduit (stream .| CC.sinkList)
    return $ head ret

getSitemap :: IO ByteString
getSitemap = get bucket_info file_sitemap

getArticle :: Text -> IO ByteString
getArticle article_id = get bucket_articles article_id
