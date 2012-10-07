module NaBot.IRCTypes
    (
      Nick(..)
    , Chan(..)
    , PingToken(..)
    , IrcMessage(..)
    , parseIrcMessage
    )

where

-- import NaBot.Config
import Text.Parsec
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

newtype Nick = Nick {unNick :: String}
    deriving (Eq, Show, Ord)

newtype Chan = Chan {unChan :: String}
    deriving (Eq, Show, Ord)

newtype PingToken = PingToken {unToken :: String}
    deriving (Eq, Show, Ord)

data IrcMessage = PING PingToken
                | PONG PingToken
                | GeneralMessage { prefix :: String
                                 , command :: String
                                 , params :: String
                                 }

instance Show IrcMessage where
    show (PING (PingToken t)) = "PING :"++t
    show (PONG (PingToken t)) = "PONG " ++t

parseIrcMessage :: String -> Either ParseError IrcMessage
parseIrcMessage = parse messageParser ""

messageParser = pingParser 

pingParser = PING <$ string "PING :" <*> (PingToken <$> many1 nospcrlfcl)
nospcrlfcl = noneOf "\0\n\r :"