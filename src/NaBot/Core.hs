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

setCurrentNick :: Nick -> BotMonad ()
setCurrentNick nick = do
  curr <- getState
  setState $ curr { currentNick = nick }
  

joinChan :: Chan -> BotMonad ()
joinChan chan = writeMessage (IRCMessage Nothing $ JOIN (unChan chan) Nothing)

handleRawMessage :: String -> BotMonad ()
handleRawMessage s = do
  liftIO $ putStrLn s
  let m = parseIrcMessage s
  case m of 
    Left err -> do
        liftIO $ putStrLn $ "COULD NOT PARSE: " ++ s
        liftIO $ putStrLn $ "BYTES: " ++ (show $ map (\c -> (c, fromEnum c)) s)
    Right (IRCMessage _ m) ->
        case m of
          (PING t) -> writeMessage $ IRCMessage Nothing $ PONG t
          (RPL_WELCOME nick _) -> do
                       setCurrentNick $ Nick nick
                       cs <- fmap (S.toList . chans) getConfig
                       mapM_ joinChan cs
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

