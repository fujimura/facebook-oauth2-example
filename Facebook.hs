{-# LANGUAGE OverloadedStrings #-}

module Facebook (
    getFacebookCred
  ) where

import qualified Config
import qualified Data.Text.Encoding    as TE
import           Network.OAuth2.OAuth2

getFacebookCred :: IO OAuth2
getFacebookCred = do
    clientId     <- Config.get "config/facebook.yml" "development" "id"
    clientSecret <- Config.get "config/facebook.yml" "development" "secret"
    callbackUrl  <- Config.get "config/facebook.yml" "development" "callback_url"

    return OAuth2 { oauthClientId            = TE.encodeUtf8 clientId
                  , oauthClientSecret        = TE.encodeUtf8 clientSecret
                  , oauthCallback            = Just $ TE.encodeUtf8 callbackUrl
                  , oauthOAuthorizeEndpoint  = "https://www.facebook.com/dialog/oauth"
                  , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"
                  , oauthAccessToken         = Nothing
                  }
