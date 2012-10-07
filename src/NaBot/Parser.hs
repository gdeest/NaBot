module NaBot.Parser
    (
     parseIrcMessage
    )

where

import NaBot.IRCTypes
import Text.Parsec
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

parseIrcMessage :: String -> Either ParseError IrcMessage
parseIrcMessage = parse messageParser ""

messageParser = pingParser 

pingParser = PING <$ string "PING :" <*> (PingToken <$> many1 nospcrlfcl)
nospcrlfcl = noneOf "\0\n\r :"