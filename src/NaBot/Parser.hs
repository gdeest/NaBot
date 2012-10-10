module NaBot.Parser
    (
     parseIrcMessage
    )

where

import NaBot.IRCTypes
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List (intersperse)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

parseIrcMessage = parse (fmap decode msg) "" 
decode (p, cmd, args) = 
    IRCMessage p $ 
               case (cmd, args) of
                 ("PING", [token])          -> PING $ PingToken token
                 ("PONG", [token])          -> PONG $ PingToken token
                 ("NOTICE", [target, text]) -> NOTICE target text
                 ("001", [nick, comment])   -> RPL_WELCOME (Nick nick) comment
                 ("JOIN", [chan])           -> JOIN (Chan chan) Nothing
                 ("JOIN", [chan, pwd])      -> JOIN (Chan chan) $ Just pwd
                 (cmd, args)                -> GenericMessage cmd args

msg = (,,) <$> optionMaybe prefix <*> command <*> args

-- Parsing prefixes
prefix :: Parser Prefix
prefix = do 
  char ':' 
  p <- try usrPrefix <|> srvPrefix
  space
  return p

-- Parses a prefix of form `nick!user@host'.
usrPrefix :: Parser Prefix
usrPrefix = mk <$> nick <* char '!' <*> user <* char '@' <*> host
    where
       mk n u h = UserPrefix (Nick n) (User u) (Hostname h)

-- Parses a server prefix composed
srvPrefix :: Parser Prefix
srvPrefix = ServerPrefix <$> fmap (concat . intersperse ".") host

-- Commands are composed exclusively of letters, or of 3 digits.
command = many1 letter <|> count 3 digit

-- Arguments
args = manyTill (do { space; x <- arg; return x }) end
    where
      arg = middle <|> trailing
      end = try $ eof <|> (char '\r' >> (return ()))
      middle = (++) <$> ((:[]) <$> nospcrlfcl) <*> many (col <|> nospcrlfcl)
      trailing = id <$ col <*> many (space <|> nospcrlfcl <|> col)

-- Components
user = many1 $ noneOf "\0\n\r @"
nick = (:) <$> (special <|> letter) <*> many1 (special <|> alphaNum <|> char '-')

host = hostname
hostname = shortname `sepBy1` (char '.')
shortname = many (noneOf " .")

-- Helpers / Char sets
special = oneOf "[]\\`_^{|}"
nospcrlfcl = noneOf "\0\n\r :"
col = char ':'
