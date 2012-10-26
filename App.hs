{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module App
    ( app
    ) where
import           Control.Applicative            (empty, (<$>), (<*>))
import           Control.Monad                  (join, when)
import           Control.Monad.Trans
import qualified Data.Aeson                     as AE
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC8
import qualified Data.ByteString.Lazy           as LBS
import           Data.Data
import           Data.Maybe                     (fromJust, isNothing)
import           Data.Monoid                    (mconcat)
import           Data.Text                      ()
import qualified Data.Text.Encoding             as E
import qualified Data.Text.Lazy                 as L
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Facebook
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types             as HT
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty                     hiding (body)

data User = User {
    userUsername :: String
  } deriving (Data, Typeable)

instance AE.FromJSON User where
  parseJSON (AE.Object v) = User <$> v AE..: "name"
  parseJSON _             = empty

data TopPage = TopPage {
  username :: String
  } deriving (Data, Typeable)

mustache :: (Data a, Typeable a) => FilePath -> a -> ActionM ()
mustache path context = do
  body <- liftIO $ decodeUtf8 <$> hastacheFile defaultConfig path (mkGenericContext context)
  html body

app :: ScottyM ()
app = do
    cred <- liftIO getFacebookCred

    get "/" $
        mustache "views/index.mustache" $ TopPage "not logged in"

    get "/auth/facebook" $
        redirect $ L.fromStrict $ E.decodeUtf8 $ authorizationUrl cred

    get "/auth/callback" $ do
        at   <- getAccessToken cred
        user <- liftIO $ doSimpleGetRequest $ "https://graph.facebook.com/me?access_token=" ++ BSC8.unpack at
        let u = fromJust $ toUser user
        mustache "views/index.mustache" $ TopPage (userUsername u)

getAccessToken :: OAuth2 -> ActionM BS.ByteString
getAccessToken cred = do
    code     <- param "code"
    response <- liftIO $ doSimpleGetRequest $ BSC8.unpack $ getAccessTokenEndpoint cred code
    let at = extractAccessToken $ responseBody response
    when (isNothing at) $ raise "Couldn't get access token" -- #TODO
    return $ fromJust at
  where
    getAccessTokenEndpoint :: OAuth2 -> BS.ByteString -> URI
    getAccessTokenEndpoint cred code = do
      let (uri, _) = accessTokenUrl' cred code Nothing
      uri `appendQueryParam` [
            ("redirect_uri"  , fromJust $ oauthCallback cred)
          , ("client_id"     , oauthClientId cred)
          , ("client_secret" , oauthClientSecret cred)
          , ("code"          , code)
        ]
    extractAccessToken :: LBS.ByteString -> Maybe BS.ByteString
    extractAccessToken body =
        join $ lookup "access_token" $ HT.parseQuery $ BS.concat . LBS.toChunks $ body

toUser :: Response LBS.ByteString -> Maybe User
toUser r = AE.decode (responseBody r) :: Maybe User
