{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

-- |
-- Module      : Network.Google.Auth.InstalledApplication
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Credentials for applications that are installed on devices such as
-- computers, cell phones, or a tablet. Installed apps are distributed to
-- individual machines, and it is assumed that these apps securely store secrets.
--
-- These apps might access a Google service while the user is present at the
-- application, or when the application is running in the background.
--
-- /See:/ <https://developers.google.com/identity/protocols/OAuth2InstalledApp Installed Application Documentation>.
module Network.Google.Auth.InstalledApplication
    ( installedApplication

    -- * Forming the URL
    , AccessType (..)
    , redirectURI
    , formURL
    , formAccessTypeURL
    , formURLWith
    , formAccessTypeURLWith

    -- * Internal Exchange and Refresh
    , exchangeCode
    , refreshToken
    ) where

import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.IO.Class         (MonadIO)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           GHC.TypeLits                   (Symbol)
import           Network.Google.Auth.Scope      (AllowScopes (..),
                                                 queryEncodeScopes)
import           Network.Google.Internal.Auth
import           Network.Google.Internal.Logger (Logger)
import           Network.Google.Prelude
import           Network.HTTP.Conduit           (Manager)
import qualified Network.HTTP.Conduit           as Client

-- | Create new Installed Application credentials.
--
-- Since it is intended that the user opens the URL generated by 'formURL' in a browser
-- and the resulting 'OAuthCode' is then received out-of-band,
-- you must ensure that the scopes passed to 'formURL' and the type of 'OAuthCode'
-- correctly match, otherwise an authorization error will occur.
--
-- For example, doing this via 'getLine' and copy-paste:
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
--
-- > import Data.Proxy     (Proxy (..))
-- > import Data.Text      as T
-- > import Data.Text.IO   as T
-- > import System.Exit    (exitFailure)
-- > import System.Info    (os)
-- > import System.Process (rawSystem)
--
-- > redirectPrompt :: AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> IO (OAuthCode s)
-- > redirectPrompt c p = do
-- >   let url = formURL c p
-- >   T.putStrLn $ "Opening URL " `T.append` url
-- >   _ <- case os of
-- >     "darwin" -> rawSystem "open"     [unpack url]
-- >     "linux"  -> rawSystem "xdg-open" [unpack url]
-- >     _        -> T.putStrLn "Unsupported OS" >> exitFailure
-- >   T.putStrLn "Please input the authorisation code: "
-- >   OAuthCode <$> T.getLine
--
-- This ensures the scopes passed to 'formURL' and the type of 'OAuthCode' 's'
-- are correct.
installedApplication :: OAuthClient -> OAuthCode s -> Credentials s
installedApplication = FromClient

-- /See:/ <https://developers.google.com/identity/protocols/OAuth2WebServer#offline>
data AccessType = Online | Offline deriving (Show, Eq)

-- | The redirection URI used in 'formURL': @urn:ietf:wg:oauth:2.0:oob@.
redirectURI :: Text
redirectURI = "urn:ietf:wg:oauth:2.0:oob"

-- | Given an 'OAuthClient' and a list of scopes to authorize,
-- construct a URL that can be used to obtain the 'OAuthCode'.
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2InstalledApp#formingtheurl Forming the URL>.
formURL :: AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> Text
formURL c = formURLWith c . allowScopes

-- | 'formURL' for 'AccessType'
--
-- /See:/ 'formUrl'.
formAccessTypeURL :: AllowScopes (s :: [Symbol]) => OAuthClient -> AccessType -> proxy s -> Text
formAccessTypeURL c a = formAccessTypeURLWith c a . allowScopes

-- | Form a URL using 'OAuthScope' values.
--
-- /See:/ 'formURL'.
formURLWith :: OAuthClient -> [OAuthScope] -> Text
formURLWith c ss = accountsURL
    <> "?response_type=code"
    <> "&client_id="    <> toQueryParam (_clientId c)
    <> "&redirect_uri=" <> redirectURI
    <> "&scope="        <> Text.decodeUtf8 (queryEncodeScopes ss)

-- | 'formURLWith' for 'AccessType'
--
-- /See:/ 'formURLWith'.
formAccessTypeURLWith :: OAuthClient -> AccessType -> [OAuthScope] -> Text
formAccessTypeURLWith c a ss = formURLWith c ss
    <> "&access_type="  <> (Text.toLower . Text.pack $ show a)

-- | Exchange 'OAuthClient' details and the received 'OAuthCode' for a new
-- 'OAuthToken'.
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2InstalledApp#handlingtheresponse Exchanging the code>.
exchangeCode :: (MonadIO m, MonadCatch m)
             => OAuthClient
             -> (OAuthCode s)
             -> Logger
             -> Manager
             -> m (OAuthToken s)
exchangeCode c n = refreshRequest $
    tokenRequest
        { Client.requestBody = textBody $
               "grant_type=authorization_code"
            <> "&client_id="     <> toQueryParam (_clientId     c)
            <> "&client_secret=" <> toQueryParam (_clientSecret c)
            <> "&code="          <> toQueryParam n
            <> "&redirect_uri="  <> redirectURI
        }

-- | Perform a refresh to obtain a valid 'OAuthToken' with a new expiry time.
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2InstalledApp#offline Refreshing tokens>.
refreshToken :: (MonadIO m, MonadCatch m)
             => OAuthClient
             -> (OAuthToken s)
             -> Logger
             -> Manager
             -> m (OAuthToken s)
refreshToken c t = refreshRequest $
    tokenRequest
        { Client.requestBody = textBody $
               "grant_type=refresh_token"
            <> "&client_id="     <> toQueryParam (_clientId     c)
            <> "&client_secret=" <> toQueryParam (_clientSecret c)
            <> maybe mempty ("&refresh_token=" <>) (toQueryParam <$> _tokenRefresh t)
        }
