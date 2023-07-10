{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Model
import qualified App
import qualified Parser

import qualified Network.Wai.Handler.Warp as Wai
import Servant
import Data.Text (Text)
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class (liftIO)
import Data.IORef ( IORef, newIORef )
import Network.Wai.Middleware.Cors ( cors, simpleCorsResourcePolicy, CorsResourcePolicy(corsRequestHeaders, corsMethods) )
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import System.Environment (getEnv)
import qualified Data.Text as T


newtype AuthInfo = AuthInfo {
  username :: Text
  } deriving (Show)

type API = "article" :> Get '[JSON] [Article]
      :<|> "article" :> Capture "path" Text :> Get '[JSON] Article
      :<|> BasicAuth "auth" AuthInfo :> "article" :> Capture "path" Text :> ReqBody '[JSON] Article :> Post '[JSON] NoContent
      :<|> BasicAuth "auth" AuthInfo :> "article" :> Capture "path" Text :> Delete '[JSON] NoContent
      
      :<|> "plain_article" :> Capture "path" Text :> Get '[JSON] Article

      :<|> "tag" :> Get '[JSON] [Text]

      :<|> "image" :> Get '[JSON] [FilePath]
      :<|> "image" :> "category" :> Get '[JSON] [FilePath]
      :<|> "image" :> Capture "category" FilePath :> Get '[JSON] [FilePath]
      :<|> BasicAuth "auth" AuthInfo :> "image" :> Capture "category" Text :> Capture "path" Text :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent

      :<|> "static" :> Raw

server :: IORef Cache -> Server API
server cache = getAllArticle :<|> getArticle :<|> upsertArticle :<|> deleteArticle
          :<|> getPlainArticle
          :<|> getTags
          :<|> getAllImages :<|> getCategory :<|> getImages :<|> uploadImage
          :<|> serveDirectoryFileServer "static"
  where getAllArticle :: Handler [Article]
        getAllArticle = liftIO $ do
          App.getAllArticleHeaders cache

        getArticle :: Text -> Handler Article
        getArticle path = liftIO $ do
          article <- App.getArticle cache path
          let body = case Model.body =<< article of
                Just a -> Right a
                Nothing -> Left "failed to get article"
          return $ case (article, Parser.parse =<< body) of
            (Just article', Right html) -> Model.updateBody article' (Just html)
            (Just article', Left err) -> Model.updateBody article' (Just err) -- これあんまりよくないかも
            (Nothing, Left err) -> error $ show err
            _ -> error "?"

        upsertArticle :: AuthInfo -> Text -> Article -> Handler NoContent
        upsertArticle _ _ article = liftIO $ do
          App.upsertArticle cache article
          return NoContent

        deleteArticle :: AuthInfo -> Text -> Handler NoContent
        deleteArticle _ path = liftIO $ do
          App.deleteArticle cache path
          return NoContent

        getPlainArticle :: Text -> Handler Article
        getPlainArticle path = liftIO $ do
          article <- App.getArticle cache path
          case article of
            Just article' -> return article'
            Nothing -> error "failed"

        getTags :: Handler [Text]
        getTags = liftIO $ do
          App.getTags cache

        getAllImages :: Handler [FilePath]
        getAllImages = liftIO $ do
          App.getAllImages cache

        getCategory :: Handler [FilePath]
        getCategory = liftIO $ do
          App.getCategory cache

        getImages :: FilePath -> Handler [FilePath]
        getImages category = liftIO $ do
          App.getImages cache category

        uploadImage :: AuthInfo -> Text -> Text -> ByteString -> Handler NoContent
        uploadImage _ category path image = liftIO $ do
          App.storeImage cache category path image
          return NoContent

api :: Proxy API
api = Proxy

app :: IORef Cache -> Text -> Application
app cache password = logStdoutDev
    $ cors (const $ Just policy)
    $ serveWithContext api ctx (server cache)
  where
  policy :: CorsResourcePolicy
  policy = simpleCorsResourcePolicy
          { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
          , corsRequestHeaders = ["Authorization", "Content-Type"] }

  ctx :: Context '[BasicAuthCheck AuthInfo]
  ctx = checkBasicAuth :. EmptyContext

  checkBasicAuth :: BasicAuthCheck AuthInfo
  checkBasicAuth = BasicAuthCheck $ \basicAuthData ->
    let username' = decodeUtf8 $ basicAuthUsername basicAuthData
        password' = decodeUtf8 $ basicAuthPassword basicAuthData
        ret = if password == password' then Authorized $ AuthInfo username' else BadPassword
    in print ret >> return ret

main :: IO ()
main = do
  password <- T.pack <$> getEnv "NOBANASHI_PASS"
  port <- read <$> getEnv "PORT" :: IO Int
  let settings = Wai.setPort port Wai.defaultSettings
  putStrLn "updateing catches..."
  cache <- newIORef =<< App.newCache
  putStrLn $ "server starting at http://localhost:" <> show port
  Wai.runSettings settings (app cache password)
