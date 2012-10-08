module NaBot.Parser
    (
     parseIrcMessage
    )

where

import NaBot.IRCTypes
import Text.Parsec
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

parseIrcMessage :: String -> Either ParseError IRCMessage
parseIrcMessage = parse messageParser ""

messageParser = IRCMessage <$> optionMaybe prefix <* space <*> body

prefix = try userPrefix <|> serverPrefix
    where
      userPrefix   = mkUserPrefix <$ (char ':') <*> nick <* (char '!') <*> user <* (char '@') <*> host
      serverPrefix = ServerPrefix <$ char ':' <*> (fmap concat host)
      mkUserPrefix n u h = UserPrefix (Nick n) (User u) (Hostname h)

nick = (special <|> letter) >> many1 (special <|> alphaNum <|> char '-')
    where 
      special = oneOf "[]\\`_^{|}"

user = many1 $ noneOf "\0\n\r @"
host = hostname
hostname = shortname `sepBy1` (string ".")

shortname = do
  fst <- (letter <|> digit) 
  rst <- do many1 (letter <|> digit <|> char '-')
  return $ fst:rst

body = pingParser

pingParser = PING <$ string "PING :" <*> (PingToken <$> many1 nospcrlfcl)
nospcrlfcl = noneOf "\0\n\r :"