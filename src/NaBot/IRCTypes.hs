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
                deriving (Eq, Show)

data Prefix = ServerPrefix String
            | UserPrefix Nick User Host
              deriving (Eq)

data Host = Hostname [String]
          deriving (Eq, Show)

data MessageBody = PING PingToken
                 | PONG PingToken
                 | NOTICE { noticeTarget :: String
                          , noticeText   :: String
                          }
                 | RPL_WELCOME { nickname :: String
                               , comment :: String
                               }
                 | GenericMessage { msgCommand :: String
                                  , msgArgs :: [String]
                                  }
                   deriving (Eq)

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
    show (RPL_WELCOME nickname comment) = "001 "++nickname++" :"++comment
    show (GenericMessage cmd args) = "GenericMessage: "++cmd++" "++ (show args)
