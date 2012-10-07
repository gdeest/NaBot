module NaBot.IRCTypes
    (
      Nick(..)
    , Chan(..)
    , PingToken(..)
    , IrcMessage(..)
    )

where

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
