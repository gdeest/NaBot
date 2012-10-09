NaBot
=====

NaBot is a typesafe library to write IRC bots.

At the moment, it cannot do much. Here is an example-bot that simply connects on a channel:

```haskell
import NaBot
import qualified Data.Set as S

cfg :: BotConfig
cfg = defaultConfig { serverHost = ServerHost "irc.freenode.net"
                    , chans      = S.fromList [Chan "#nabot"]
                    }

main :: IO ()
main = runBot cfg
```
