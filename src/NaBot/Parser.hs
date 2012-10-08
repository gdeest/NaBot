module NaBot.Parser
    -- (
    --  parseIrcMessage
    -- )das

where

import NaBot.IRCTypes
import Text.Parsec
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

parseIrcMessage :: String -> Either ParseError IRCMessage
parseIrcMessage = parse messageParser ""

messageParser = IRCMessage <$> optionMaybe prefix <*> body

prefix = do
  result <- try userPrefix <|> try serverPrefix
  space
  return result

userPrefix   = mkUserPrefix <$ (char ':') <*> nick <* (char '!') <*> user <* (char '@') <*> host
    where
      mkUserPrefix n u h = UserPrefix (Nick n) (User u) (Hostname h)

serverPrefix = ServerPrefix <$ char ':' <*> (fmap concat host)
     

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

body = try pingParser <|>
       try noticeParser <|>
       try welcomeParser

pingParser = PING <$ string "PING :" <*> (PingToken <$> many1 nospcrlfcl)
twoParams cons s = cons <$ string (s++ " ") <*> many1 nospcrlfcl <* string " :" <*> many anyChar

noticeParser  = twoParams NOTICE      "NOTICE"
welcomeParser = twoParams RPL_WELCOME "001"

nospcrlfcl = noneOf "\0\n\r :"