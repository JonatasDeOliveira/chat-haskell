{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Types where

import GHC.Generics
import Data.Binary
import Data.Typeable.Internal
import Data.Map (Map)
import Control.Distributed.Process (SendPort)


type ChatName = String

type NickName = String

type Host = String

type ServerAddress = String

type ClientPortMap = Map NickName (SendPort ChatMessage)
type Conections = Map NickName NickName

type ClientLogMap = Map NickName Bool

data Sender = Server | Client NickName
  deriving (Generic, Typeable, Eq, Show)

instance Binary Sender

data ChatMessage = ChatMessage {
    from :: Sender
  , dest :: NickName
  , message :: String
  } deriving (Generic, Typeable, Show)

instance Binary ChatMessage

data JoinChatMessage = JoinChatMessage {
	clientName :: NickName
  , clientNameDest :: NickName 
  } deriving (Generic, Typeable, Show)

data SignUpMessage = SignUpMessage {
	clientName :: NickName
  } deriving (Generic, Typeable, Show)

instance Binary JoinChatMessage

instance Binary SignUpMessage