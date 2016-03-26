{-# LANGUAGE OverloadedStrings #-}

module Network.Docker
  (
  -- * Docker Operations
  -- ** Version
    getDockerVersion
  -- ** Containers

  -- *** Creating and Deleting
  , createContainer
  , deleteContainer
  , deleteContainerWithOpts
  -- *** Starting, Stopping, Pausing
  , startContainer
  , stopContainer
  , killContainer
  , restartContainer
  , pauseContainer
  , unpauseContainer
  -- *** Getting Container Information
  , getDockerContainers
  , getAllDockerContainers
  -- *** Logs
  , getContainerLogs
  , getContainerLogsStream
  -- ** Images
  , getAllDockerImages
  , getDockerImages
  -- * Types
  , module Network.Docker.Types
  ) where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson                  (FromJSON, ToJSON, decode, toJSON)
import           Data.Aeson.Lens             (key, _String, AsValue)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Text                   as T
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq
import           OpenSSL.Session             (SSLContext)
import qualified OpenSSL.Session             as SSL
import           Pipes
import qualified Pipes.ByteString            as PB
import qualified Pipes.HTTP                  as PH
import           Text.Printf                 (printf)

import           Network.Docker.Internal
import           Network.Docker.Types

defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts
                { apiVersion = "v1.12"
                , baseUrl = "http://127.0.0.1:3128/"
                , ssl = NoSSL
                }

getDockerVersion :: DockerClientOpts -> IO (Maybe DockerVersion)
getDockerVersion = decodeResponse . _dockerGetQuery "/version"

-- | Get running docker containers
getDockerContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
getDockerContainers = decodeResponse . _dockerGetQuery "/containers/json"

-- | Get all docker containers (running and not running)
getAllDockerContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
getAllDockerContainers = decodeResponse . _dockerGetQuery "/containers/json?all=true"

getDockerImages :: DockerClientOpts -> IO (Maybe [DockerImage])
getDockerImages = decodeResponse . _dockerGetQuery "/images/json"

getAllDockerImages :: DockerClientOpts -> IO (Maybe [DockerImage])
getAllDockerImages = decodeResponse . _dockerGetQuery "/images/json?all=true"

createContainer :: DockerClientOpts -> CreateContainerOpts -> IO (Maybe T.Text)
createContainer clientOpts createOpts = getOutOfResponse "Id" <$> (_dockerPostQuery "/containers/create" clientOpts createOpts)

startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO (Status)
startContainer clientOpts containerId startOpts = (^. responseStatus) <$> _dockerPostQuery (printf "/containers/%s/start" containerId) clientOpts startOpts

stopContainer :: DockerClientOpts -> String -> IO (Status)
stopContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/stop" containerId) clientOpts

killContainer :: DockerClientOpts -> String -> IO (Status)
killContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/kill" containerId) clientOpts

restartContainer :: DockerClientOpts -> String -> IO (Status)
restartContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/restart" containerId) clientOpts

pauseContainer :: DockerClientOpts -> String -> IO (Status)
pauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/pause" containerId) clientOpts

unpauseContainer :: DockerClientOpts -> String -> IO (Status)
unpauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/unpause" containerId) clientOpts

deleteContainer :: DockerClientOpts -> String -> IO (Status)
deleteContainer dco = deleteContainerWithOpts dco defaultDeleteOpts

deleteContainerWithOpts :: DockerClientOpts -> DeleteOpts ->  String -> IO (Status)
deleteContainerWithOpts clientOpts (DeleteOpts removeVolumes force) containerId =
  (^. responseStatus) <$> _dockerEmptyDeleteQuery req clientOpts
  where req = printf "/containers/%s?v=%s;force=%s"
                         containerId
                         (show removeVolumes)
                         (show force)

-- | Get the logs of a container and stream it to stdout. Keeps the connection
-- alive and streams new log entries as they arise (follow mode)
getContainerLogsStream :: DockerClientOpts -> String -> IO ()
getContainerLogsStream  clientOpts containerId = do
                req <- PH.parseUrl (fullUrl clientOpts url)
                let req' =  req {PH.method = "GET"}
                m <- PH.newManager PH.defaultManagerSettings
                PH.withHTTP req' m  $
                  \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
        where url = (printf "/containers/%s/logs?stdout=1&stderr=1&follow=1"
                            containerId)

-- | Get the logs of a container as a Lazy ByteString.
getContainerLogs :: DockerClientOpts -> String -> IO (L.ByteString)
getContainerLogs  clientOpts containerId = (^. responseBody) <$> _dockerGetQuery url clientOpts
        where url = (printf "/containers/%s/logs?stdout=1&stderr=1" containerId)
