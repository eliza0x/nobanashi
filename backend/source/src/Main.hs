{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Model
import qualified Parser as P
import qualified Storage as S

import qualified Network.Wai.Handler.Warp as Wai
import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors ( cors, simpleCorsResourcePolicy, CorsResourcePolicy(corsRequestHeaders, corsMethods) )
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Set as Set

newtype AuthInfo = AuthInfo {
  username :: Text
  } deriving (Show)

type API = Get '[JSON] Text
      :<|> "article" :> Get '[JSON] [ArticleInfo]
      :<|> "article" :> Capture "path" Text :> Get '[JSON] Article
      :<|> BasicAuth "auth" AuthInfo :> "article" :> Capture "path" Text :> ReqBody '[JSON] Article :> Post '[JSON] NoContent

      :<|> "plain_article" :> Capture "path" Text :> Get '[JSON] Article

      :<|> "tag" :> Get '[JSON] [Text]

server :: Server API
server = getRoot
          :<|> getAllArticle :<|> getArticle :<|> uploadArticle
          :<|> getPlainArticle
          :<|> getTags
  where getRoot :: Handler Text
        getRoot = return "こんにちは"

        getAllArticle :: Handler [ArticleInfo]
        getAllArticle = liftIO S.getInfos

        getArticle :: Text -> Handler Article
        getArticle path = liftIO $ do
          article <- S.getArticle path
          return $ case P.parse (Model.body article) of
            Right html -> Model.Article (Model.info article) html
            Left err -> Model.Article (Model.info article) err -- これあんまりよくないかも

        uploadArticle :: AuthInfo -> Text -> Article -> Handler NoContent
        uploadArticle _ _ article = liftIO $ do
          S.uploadArticle article
          return NoContent

        getPlainArticle :: Text -> Handler Article
        getPlainArticle path = liftIO $ do
          S.getArticle path

        getTags :: Handler [Text]
        getTags = liftIO (Set.toList . Set.fromList . concatMap Model.tags <$> S.getInfos)

api :: Proxy API
api = Proxy

app :: Text -> Application
app password = logStdoutDev
    $ cors (const $ Just policy)
    $ serveWithContext api ctx server
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
  -- port <- read <$> getEnv "PORT" :: IO Int
  port <- return 8080 :: IO Int

  let settings = Wai.setPort port Wai.defaultSettings
  putStrLn "updateing catches..."
  putStrLn $ "server starting at http://localhost:" <> show port
  Wai.runSettings settings (app password)
