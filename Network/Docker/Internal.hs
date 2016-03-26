module Network.Docker.Internal where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson                  (FromJSON, ToJSON, decode, toJSON)
import           Data.Aeson.Lens             (key, _String, AsValue)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Text                   as T
import           Network.Docker.Types
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq
import           OpenSSL.Session             (SSLContext)
import qualified OpenSSL.Session             as SSL
import           Text.Printf                 (printf)

constructUrl :: URL -> ApiVersion -> Endpoint -> URL
constructUrl url apiVersion endpoint = printf "%s%s%s" url apiVersion endpoint

constructRelativeUrl :: String -> String
constructRelativeUrl url = url :: String

decodeResponse :: (Functor f, FromJSON a) =>
                  f (Response L.ByteString)
               -> f (Maybe a)
decodeResponse r = decode <$> (^. responseBody) <$> r

getOutOfResponse :: AsValue body => T.Text -> Response body -> Maybe T.Text
getOutOfResponse k r = (^? responseBody . key k . _String) r

getResponseStatusCode :: Response body -> Status
getResponseStatusCode r = (^. responseStatus) r

fullUrl :: DockerClientOpts -> Endpoint -> URL
fullUrl clientOpts endpoint = constructUrl (baseUrl clientOpts) (apiVersion clientOpts) endpoint

setupSSLCtx :: SSLOptions -> IO SSLContext
setupSSLCtx (SSLOptions key cert) = do
  ctx <- SSL.context
  SSL.contextSetPrivateKeyFile  ctx key
  SSL.contextSetCertificateFile ctx cert
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  return ctx


mkOpts :: IO SSLContext -> Network.Wreq.Options
mkOpts c = defaults & manager .~ Left (opensslManagerSettings c)

getSSL
  :: SSLOptions
  -> String
  -> IO (Response L.ByteString)
getSSL sopts url = withOpenSSL $ getWith (mkOpts $ setupSSLCtx sopts) url

postSSL
  :: ToJSON a
  => SSLOptions
  -> String
  -> a
  -> IO (Response L.ByteString)
postSSL sopts url = withOpenSSL . postWith (mkOpts $ setupSSLCtx sopts) url . toJSON

_dockerGetQuery :: Endpoint -> DockerClientOpts -> IO(Response L.ByteString)
_dockerGetQuery endpoint clientOpts@DockerClientOpts{ssl = NoSSL} =
  get (fullUrl clientOpts endpoint)
_dockerGetQuery endpoint clientOpts@DockerClientOpts{ssl = SSL sslOpts} =
  getSSL sslOpts (fullUrl clientOpts endpoint)

_dockerPostQuery :: ToJSON a => Endpoint -> DockerClientOpts -> a -> IO (Response L.ByteString)
_dockerPostQuery endpoint clientOpts@DockerClientOpts{ssl = NoSSL} postObject =
  post (fullUrl clientOpts endpoint) (toJSON postObject)
_dockerPostQuery endpoint clientOpts@DockerClientOpts{ssl = SSL sslOpts} postObject =
  postSSL sslOpts (fullUrl clientOpts endpoint) postObject

emptyPost :: String
emptyPost = ""

_dockerEmptyPostQuery :: Endpoint -> DockerClientOpts -> IO (Response L.ByteString)
_dockerEmptyPostQuery endpoint clientOpts = post (fullUrl clientOpts endpoint) (toJSON emptyPost)

_dockerEmptyDeleteQuery :: Endpoint
                        -> DockerClientOpts
                        -> IO (Response L.ByteString)
_dockerEmptyDeleteQuery endpoint clientOpts = delete (fullUrl clientOpts endpoint)
