module NaBot.IRCTypes
    (
      Nick(..)
    , Chan(..)
    , User(..)
    , PingToken(..)
    , IRCMessage(..)
    , Prefix(..)
    , MessageBody(..)
    , Host(..)
    )

where

import Data.List (intersperse)

newtype Nick = Nick {unNick :: String}
    deriving (Eq, Show)

newtype User = User {unUser :: String}
    deriving (Eq, Show)

newtype Chan = Chan {unChan :: String}
    deriving (Eq, Show, Ord)

newtype PingToken = PingToken {unToken :: String}
    deriving (Eq, Show)

data IRCMessage = IRCMessage { messagePrefix :: Maybe Prefix
                             , messageBody   :: MessageBody
                             }

data Prefix = ServerPrefix String
            | UserPrefix Nick User Host

data Host = Hostname [String]

data MessageBody = PING PingToken
                 | PONG PingToken

instance Show Prefix where
    show prefix = case prefix of
                    ServerPrefix s -> s
                    UserPrefix n u h ->
                        (unNick n) ++ "!" ++ (unUser u) ++ "@" ++ h'
                            where h' = 
                                      case h of
                                        Hostname hs ->
                                            concat $ intersperse "." hs

instance Show MessageBody where
    show (PING (PingToken t)) = "PING :"++t
    show (PONG (PingToken t)) = "PONG " ++t
