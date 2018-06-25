module Client where

import Control.Distributed.Process.ManagedProcess.Client (callChan, cast)
import Control.Distributed.Process ( expectTimeout
                                   , whereisRemoteAsync
                                   , spawnLocal
                                   , receiveChan
                                   , link
                                   , NodeId(..)
                                   , Process
                                   , ProcessId
                                   , ReceivePort
                                   , WhereIsReply(..) )
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode )
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport     (EndPointAddress(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad (void, forever)
import qualified Data.ByteString.Char8 as BS (pack)
import Types
import Logger (runChatLogger, logChatMessage, logStr)


searchChatServer :: ChatName -> ServerAddress -> Process ProcessId
searchChatServer name serverAddr = do
  let addr = EndPointAddress (BS.pack serverAddr)
      srvId = NodeId addr
  whereisRemoteAsync srvId name
  reply <- expectTimeout 1000
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return sid
    _ -> searchChatServer name serverAddr

launchChatClient :: ServerAddress -> Host -> Int -> ChatName -> IO ()
launchChatClient serverAddr clientHost port name  = do
  mt <- createTransport clientHost (show port) defaultTCPParameters
  case mt of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runChatLogger node
      runProcess node $ do
        serverPid <- searchChatServer name serverAddr
        link serverPid
        logStr "Iniciando chat ... "
        logStr "Por favor, digite seu usuário ... "
        nickName <- liftIO getLine
        rp <- callChan serverPid (SignUpMessage nickName) :: Process (ReceivePort ChatMessage)
        (ChatMessage from dest msg) <- receiveChan rp
        logStr msg
        nickNameDest <- getDestClientLoop serverPid nickName rp
        void $ spawnLocal $ forever $ do
            msg <- receiveChan rp
            logChatMessage msg
        forever $ do
            chatInput <- liftIO getLine
            cast serverPid (ChatMessage (Client nickName) nickNameDest chatInput)
            liftIO $ threadDelay 500000
        
getDestClientLoop :: ProcessId -> NickName -> ReceivePort ChatMessage -> Process String
getDestClientLoop serverPid nickName rp = do
  logStr "Digite o nome do usuário com quem quer conversar"
  nickNameDest <- liftIO getLine
  cast serverPid (JoinChatMessage nickName nickNameDest)
  (ChatMessage from dest msg) <- receiveChan rp
  if msg == "error1"
    then logStr ("Não existe o usuário " ++ nickNameDest) >> getDestClientLoop serverPid nickName rp
    else do
      if msg == "error2"
        then logStr (nickNameDest ++ " está offline ...") >> getDestClientLoop serverPid nickName rp
        else do
          logStr ("Conectado ao usuário " ++ nickNameDest)
          return nickNameDest


