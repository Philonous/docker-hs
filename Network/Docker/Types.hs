{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Docker.Types where

import           Control.Applicative
import           Control.Lens.TH        hiding (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bool
import qualified Data.Text              as T
import           Network.Docker.Options
import           Prelude                hiding (id)

import           Network.Docker.Utils

type URL = String
type ApiVersion = String
type Endpoint = String

type Tag = String
type IP = String
type Port = Int
type PortType = String

data SSL = NoSSL | SSL SSLOptions deriving Show

data DockerClientOpts = DockerClientOpts {
      apiVersion :: ApiVersion
    , baseUrl    :: URL
    , ssl        :: SSL
    } deriving (Show)


data SSLOptions = SSLOptions {
    optionsKey  :: FilePath
  , optionsCert :: FilePath
  } deriving Show


newtype ResourceId = ResourceId { _id :: String } deriving (Show, Eq)

makeClassy ''ResourceId

data DockerImage = DockerImage
                { _imageId        :: ResourceId
                , _imageCreatedAt :: Int
                , _parentId       :: Maybe String
                , _repoTags       :: [Tag]
                , _size           :: Int
                , _virtualSize    :: Int
                } deriving (Show, Eq)

instance FromJSON DockerImage where
        parseJSON = withObject "docker image" $ \v ->
            DockerImage <$> ResourceId <$> (v .: "Id")
                        <*> (v .: "Created")
                        <*> (v .:? "ParentId")
                        <*> (v .: "RepoTags")
                        <*> (v .: "Size")
                        <*> (v .: "VirtualSize")

makeLenses ''DockerImage

-- instance HasResourceId DockerImage where
--         resourceId = imageId

data DockerVersion = DockerVersion
                  { _Version       :: String
                  , _GitCommit     :: String
                  , _GoVersion     :: String
                  , _Arch          :: String
                  , _KernelVersion :: String
                  } deriving (Show, Eq)

makeLenses ''DockerVersion
deriveJSON dopts ''DockerVersion

-- The JSON looks likes this:
-- "Ports":[{"IP":"0.0.0.0","PrivatePort":55555,"PublicPort":55555,"Type":"tcp"}]

data PortMap = PortMap
            { _ip          :: IP
            , _privatePort :: Port
            , _publicPort  :: Port
            , _type        :: PortType
            } deriving (Show, Eq)

makeLenses ''PortMap

instance FromJSON PortMap where
        parseJSON = withObject "portmap" $ \v ->
            PortMap <$> (v .: "IP")
                    <*> (v .: "PrivatePort")
                    <*> (v .: "PublicPort")
                    <*> (v .: "Type")


data DeleteOpts = DeleteOpts
            { removeVolumes :: Bool
            , force         :: Bool
            }

makeLenses ''DeleteOpts

defaultDeleteOpts :: DeleteOpts
defaultDeleteOpts = DeleteOpts False False


data DockerContainer = DockerContainer
                    { _containerId        :: ResourceId
                    , _containerImageId   :: ResourceId
                    , _command            :: String
                    , _containerCreatedAt :: Int
                    , _names              :: [String]
                    , _status             :: String
                    , _ports              :: Maybe [PortMap]
                    } deriving (Show, Eq)

instance FromJSON DockerContainer where
        parseJSON = withObject "docker container" $ \v ->
            DockerContainer <$> (ResourceId <$> (v .: "Id"))
                            <*> (ResourceId <$> (v .: "Id"))
                            <*> (v .: "Command")
                            <*> (v .: "Created")
                            <*> (v .: "Names")
                            <*> (v .: "Status")
                            <*> (v .:? "Ports")

makeLenses ''DockerContainer

instance HasResourceId DockerContainer where
        resourceId = containerId


data CreateContainerOpts = CreateContainerOpts
                  { _hostname       :: String
                  , _user           :: String
                  , _memory         :: Int
                  , _memorySwap     :: Int
                  , _attachStdin    :: Bool
                  , _attachStdout   :: Bool
                  , _attachStderr   :: Bool
                  , _portSpecs      :: Maybe Object
                  , _tty            :: Bool
                  , _openStdin      :: Bool
                  , _stdinOnce      :: Bool
                  , _env            :: Maybe Object
                  , _cmd            :: [String]
                  , _image          :: String
                  , _volumes        :: Maybe Object
                  , _volumesFrom    :: Maybe Object
                  , _workingDir     :: String
                  , _disableNetwork :: Bool
                  , _exposedPorts   :: Maybe Object
                  } deriving (Show)

makeLenses ''CreateContainerOpts

defaultCreateOpts :: CreateContainerOpts
defaultCreateOpts = CreateContainerOpts {
                             _hostname = ""
                            , _user = ""
                            , _memory = 0
                            , _memorySwap =  0
                            , _attachStdin = False
                            , _attachStdout = False
                            , _attachStderr = False
                            , _portSpecs = Nothing
                            , _tty = False
                            , _openStdin =  False
                            , _stdinOnce = False
                            , _env = Nothing
                            , _cmd = []
                            , _image = "debian"
                            , _volumes = Nothing
                            , _volumesFrom =  Nothing
                            , _workingDir = ""
                            , _disableNetwork = False
                            , _exposedPorts = Nothing
                            }

instance ToJSON CreateContainerOpts where
        toJSON (CreateContainerOpts {..}) = object
            [ "Hostname" .= _hostname
            , "User" .= _user
            , "Memory" .= _memory
            , "MemorySwap" .= _memorySwap
            , "AttachStdin" .= _attachStdin
            , "AttachStdout" .= _attachStdout
            , "AttachStderr" .= _attachStderr
            , "PortSpecs" .= _portSpecs
            , "Tty" .= _tty
            , "OpenStdin" .= _openStdin
            , "StdinOnce" .= _stdinOnce
            , "Env" .= _env
            , "Cmd" .= _cmd
            , "Image" .= _image
            , "Volumes" .= _volumes
            , "VolumesFrom" .= _volumesFrom
            , "WrokingDir" .= _workingDir
            , "DisableNetwork" .= _disableNetwork
            , "ExposedPorts" .= _exposedPorts
            ]

data RestartPolicy = RestartNever
                   | RestartAlways
                   | RestartOnFailure Int
                   deriving (Show)

makePrisms ''RestartPolicy

data StartContainerOpts = StartContainerOpts
                        { _Binds           :: [T.Text]
                        , _Links           :: [T.Text]
                        , _LxcConf         :: [(T.Text, T.Text)]
                        , _PortBindings    :: [((Int,T.Text),Int)]
                        , _PublishAllPorts :: Bool
                        , _Privileged      :: Bool
                        , _Dns             :: [T.Text]
                        , _VolumesFrom     :: [T.Text]
                        , _RestartPolicy   :: RestartPolicy
                        } deriving (Show)

makeLenses ''StartContainerOpts

defaultStartOpts :: StartContainerOpts
defaultStartOpts = StartContainerOpts
                { _Binds = []
                , _Links = []
                , _LxcConf = []
                , _PortBindings = []
                , _PublishAllPorts = False
                , _Privileged = False
                , _Dns = []
                , _VolumesFrom = []
                , _RestartPolicy = RestartNever
                }

instance ToJSON StartContainerOpts where
        toJSON (StartContainerOpts {..}) = object
            [ "Binds" .= _Binds
            , "Links" .= _Links
            , "LxcConf" .= _LxcConf
            , "PortBindings" .= object (map (\((h,p),c)->T.concat [T.pack (show h),"/",p] .= [object ["HostPort" .= show c]]) _PortBindings)
            , "PublishAllPorts" .= _PublishAllPorts
            , "Privileged" .= _Privileged
            , "Dns" .= _Dns
            , "VolumesFrom" .= _VolumesFrom
            , "RestartPolicy" .= _RestartPolicy
            ]


instance ToJSON RestartPolicy where
  toJSON RestartNever = object [ "Name" .= (""::String)
                               , "MaximumRetryCount" .= (0::Int)
                               ]
  toJSON RestartAlways = object [ "Name" .= ("always"::String)
                                , "MaximumRetryCount" .= (0::Int)
                                ]
  toJSON (RestartOnFailure n) = object [ "Name" .= ("on-failure"::String)
                                       , "MaximumRetryCount" .= n
                                       ]
