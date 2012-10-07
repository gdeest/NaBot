module NaBot.Config 
    (
      BotConfig(..)
    , defaultConfig
    , ServerHost(..)
    , ServerPort(..)
    )

where

import NaBot.IRCTypes
import Control.Exception
import qualified Data.Set as S

newtype ServerHost = ServerHost {unHost :: String} 
    deriving (Eq, Show, Ord)

newtype ServerPort = ServerPort {unPort :: Int}
    deriving (Eq, Show, Ord)

data BotConfig = BotConfig { serverHost :: ServerHost
                           , serverPort :: ServerPort
                           , botNick    :: Nick
                           , chans      :: S.Set Chan
                           , onError    :: IOException -> IO ()
                           }

defaultConfig :: BotConfig
defaultConfig = BotConfig
                (ServerHost "localhost")
                (ServerPort 6667)
                (Nick "NaBot")
                (S.fromList $ map Chan [])
                (\e -> return ())