{-# LANGUAGE RecordWildCards #-}

module Server where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.ManagedProcess ( serve
                                                  , defaultProcess
                                                  , handleRpcChan
                                                  , handleCast
                                                  , handleInfo
                                                  , InitResult(..)
                                                  , UnhandledMessagePolicy(..)
                                                  , ChannelHandler
                                                  , ActionHandler
                                                  , CastHandler
                                                  , ProcessDefinition(..) )
import Control.Distributed.Process ( spawnLocal
                                   , register
                                   , monitorPort
                                   , sendPortId
                                   , processNodeId
                                   , Process
                                   , DiedReason(..)
                                   , ProcessId(..)
                                   , NodeId(..)
                                   , PortMonitorNotification(..) )
import Control.Distributed.Process.ManagedProcess.Server (replyChan, continue)
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode )
import Control.Concurrent (threadDelay)
import Network.Socket.Internal (withSocketsDo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, forM_, void)
import Logger (runChatLogger, logStr)
import qualified Data.Map as M (insert, empty, member, delete, filter, elemAt, filterWithKey, elems, lookup, keys)
import Types
import Data.Maybe (fromMaybe)

serveChatRoom :: Host -> Int -> ChatName -> IO ()
serveChatRoom host port name = withSocketsDo $ do
  mt <- createTransport host (show port) defaultTCPParameters
  case mt of
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runChatLogger node
      runProcess node $ do
        pId <- launchChatServer
        logStr $ "Server launched at: " ++ show (nodeAddress . processNodeId $ pId)
        register name pId
        liftIO $ forever $ threadDelay 500000
    Left err -> print err

broadcastMessage :: ClientPortMap -> ChatMessage -> Process ()
broadcastMessage clientPorts msg =
  forM_ (filtering clientPorts msg) (`replyChan` msg)

filtering :: ClientPortMap -> ChatMessage -> ClientPortMap
filtering clientPorts ChatMessage{..} = M.filterWithKey (\x _ -> x==dest) clientPorts


messageHandler :: CastHandler (ClientLogMap,ClientPortMap,Conections) ChatMessage
messageHandler = handler
  where
    handler :: ActionHandler (ClientLogMap,ClientPortMap,Conections) ChatMessage
    handler (logClients,clients,conections) msg = do
      broadcastMessage clients msg
      continue (logClients,clients,conections)

joinChatHandler :: CastHandler (ClientLogMap,ClientPortMap,Conections) JoinChatMessage
joinChatHandler = handler
  where
    handler :: ActionHandler (ClientLogMap,ClientPortMap,Conections) JoinChatMessage
    handler (logClients,clients,conections) JoinChatMessage{..} = do
      case M.lookup clientNameDest logClients of
        (Just True) -> do
          let msg = clientName ++ " iniciou um chat com " ++ clientNameDest
              conections' = M.insert clientName clientNameDest conections
              conections'' = M.insert clientNameDest clientName conections'
          logStr msg
          broadcastMessage clients $ ChatMessage Server clientName msg
          continue (logClients,clients,conections'')
        (Just False) -> do
          let msg = "$error2$"
          logStr msg
          broadcastMessage clients $ ChatMessage Server clientName msg
          continue (logClients,clients,conections)
        Nothing -> do
          let msg = "$error1$"
          logStr msg
          broadcastMessage clients $ ChatMessage Server clientName msg
          continue (logClients,clients,conections)


loggedHandler :: CastHandler (ClientLogMap,ClientPortMap,Conections) LoggedMessage
loggedHandler = handler
  where
    handler :: ActionHandler (ClientLogMap,ClientPortMap,Conections) LoggedMessage
    handler (logClients,clients,conections) LoggedMessage{..} = do
      let msg = onlineUsersMessage logClients from
      broadcastMessage clients $ ChatMessage Server from msg
      continue (logClients,clients,conections)
          

signUpHandler :: ChannelHandler (ClientLogMap,ClientPortMap,Conections) SignUpMessage ChatMessage
signUpHandler sendPort = handler
  where
    handler :: ActionHandler (ClientLogMap,ClientPortMap,Conections) SignUpMessage
    handler (logClients,clients,conections) SignUpMessage{..} =
      if not $ clientName `M.member` clients
        then do
          void $ monitorPort sendPort
          let clients' = M.insert clientName sendPort clients
              msg = clientName ++ " adicionado ao sistema ..."
              logClients' = M.insert clientName True logClients
          logStr msg
          broadcastMessage clients' $ ChatMessage Server clientName msg
          continue (logClients',clients', conections)
        else do
          void $ monitorPort sendPort
          logStr (clientName ++ " reconectou ...")
          let msg = "Bem vindo de volta " ++ clientName
              clients' = M.insert clientName sendPort clients
              logClients' = M.insert clientName True logClients
          broadcastMessage clients' $ ChatMessage Server clientName msg
          continue (logClients',clients',conections)

disconnectHandler :: ActionHandler (ClientLogMap,ClientPortMap,Conections) PortMonitorNotification
disconnectHandler (logClients,clients,conections) (PortMonitorNotification _ spId reason) = do

  let search = M.filter (\v -> sendPortId v == spId) clients

  case (null search, reason) of
    (True, _) -> continue (logClients,clients,conections)
    _ -> do
      let clientName = head (M.keys search)
          logClients' = M.insert clientName False logClients
          clientNameDest = fromMaybe "" (M.lookup clientName conections)
          conections' = M.delete clientName conections
          conections'' = M.delete clientNameDest conections'
          msg = "$error3$"++clientName++"$" 
      broadcastMessage clients $ ChatMessage Server clientNameDest msg
      logStr (clientName ++ " deslogou ...")
      continue (logClients',clients,conections'')

launchChatServer :: Process ProcessId
launchChatServer =
  let server = defaultProcess {
          apiHandlers =  [ handleCast joinChatHandler
                         , handleRpcChan signUpHandler
                         , handleCast messageHandler
                         , handleCast loggedHandler
                         ]
        , infoHandlers = [ handleInfo disconnectHandler ]
        , unhandledMessagePolicy = Log
        }
  in spawnLocal $ serve () (const (return $ InitOk (M.empty,M.empty,M.empty) Infinity)) server


onlineUsersMessage :: ClientLogMap -> String -> String
onlineUsersMessage logClients from = let users = (filter (\v -> v/=from) (M.keys logClients)) 
                                     in 
                                      if (length users) == 0
                                        then "Não há outros usuários no sistema ..."
                                        else helperLogUsers users logClients ""

helperLogUsers :: [NickName] -> ClientLogMap -> String -> String
helperLogUsers [] _ s = s
helperLogUsers (x:xs) logClients s = case (M.lookup x logClients) of
                            (Just True) -> helperLogUsers xs logClients (s ++ x ++ " -> on\n")
                            _ -> helperLogUsers xs logClients (s ++ x ++ " -> off\n")

