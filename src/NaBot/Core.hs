{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NaBot.Core
    (
     runBot
    )

where

import NaBot.Config
import NaBot.IRCTypes
import NaBot.Parser
import Prelude hiding (catch)
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Data.List (isPrefixOf)
import qualified Data.Set as S
import Network
import System.IO
import Text.Printf

data BotData = BotData { botConfig :: BotConfig
                       , botHandle :: Handle
                       }

data BotState = BotState { currentChans :: S.Set Chan
                         , currentNick  :: Nick
                         }

newtype GBotMonad m a = BotMonad { unBotMonad :: ReaderT BotData (StateT BotState m) a }
    deriving (Functor, Monad, MonadIO)

type BotMonad a = GBotMonad IO a

runBotMonad :: BotData -> BotState -> BotMonad a -> IO a
runBotMonad botInfo botState action = evalStateT (runReaderT (unBotMonad action) botInfo) botState

initBotState :: Nick -> BotState
initBotState nick = BotState (S.fromList []) nick

getHandle :: BotMonad Handle
getHandle = liftM botHandle (BotMonad ask)

getConfig :: BotMonad BotConfig
getConfig = liftM botConfig (BotMonad ask)

getState :: BotMonad BotState
getState = BotMonad (lift get)

setState :: BotState -> BotMonad ()
setState st = BotMonad (lift $ put st)

write :: String -> String -> BotMonad ()
write s t = do
  h <- getHandle
  liftIO $ hPrintf h "%s %s\r\n" s t


writeMessage :: IRCMessage -> BotMonad ()
writeMessage m = do
  h <- getHandle
  let (IRCMessage _ msg) = m
  let str = show msg
  liftIO $ do
    printf "> %s\n" str
    hPrintf h "%s\r\n" str

getCurrentNick :: BotMonad Nick
getCurrentNick = do
  fmap currentNick getState

setCurrentNick :: Nick -> BotMonad ()
setCurrentNick nick = do
  curr <- getState
  setState $ curr { currentNick = nick }
  
joinChan :: Chan -> BotMonad ()
joinChan chan = writeMessage $ IRCMessage Nothing $ JOIN chan Nothing

addToCurrentChans :: Chan -> BotMonad ()
addToCurrentChans chan = do
  curr <- getState
  let currChans = currentChans curr
      newChans  = S.insert chan currChans
  setState $ curr { currentChans = newChans }

handleRawMessage :: String -> BotMonad ()
handleRawMessage s = do
  liftIO $ putStrLn s
  let msg = parseIrcMessage s
  case msg of 
    Left err -> do
        liftIO $ putStrLn $ "COULD NOT PARSE: " ++ s
        liftIO $ putStrLn $ "BYTES: " ++ (show $ map (\c -> (c, fromEnum c)) s)
    Right (IRCMessage prefix body) ->
        case body of
          (PING t) -> writeMessage $ IRCMessage Nothing $ PONG t
          (RPL_WELCOME nick _) -> do
                       setCurrentNick  nick
                       cs <- fmap (S.toList . chans) getConfig
                       mapM_ joinChan cs
          (JOIN chan Nothing) -> 
              case prefix of
                Just (UserPrefix n u h) -> 
                    do
                      currNick <- getCurrentNick
                      if currNick == n
                        then do
                          liftIO $ putStrLn $ "Joined chan: " ++ (unChan chan)
                          addToCurrentChans chan
                        else return ()
                _                       -> return ()
          _        -> return ()

handshake :: BotMonad ()
handshake = do
  nick <- getConfig >>= return . unNick . botNick
  write "NICK" nick
  write "USER" (nick++" 0 * :NaBot IRC Bot")

mainLoop :: BotMonad ()
mainLoop = forever $ do
  h <- getHandle
  s <- liftIO $ hGetLine h
  handleRawMessage s

connect :: BotConfig -> IO ()
connect config = do
  let host = unHost $ serverHost config
      port = PortNumber $ fromIntegral $ unPort $ serverPort config
  h <- connectTo host port
  hSetBuffering h LineBuffering
  let bInfo = BotData config h
      bState = initBotState $ botNick config
  runBotMonad bInfo bState $ handshake >> mainLoop

runBot :: BotConfig -> IO ()
runBot config = forever $ (connect config) `catch` (onError config)

