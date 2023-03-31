module App where

import           Data.Text              as T
import           Data.Text.IO           as T
import           Discord.Internal.Types (UserId)

-- TODO: Config with modifiable Markov state. Also chains can probably be done without state using a database
readEnv :: IO [Text]
readEnv = do
  file <- T.readFile ".env"
  return $ T.lines file

-- TODO: Read these from .env
botPrefix :: Char
botPrefix = '%'

botId :: UserId
botId = 1042440644028813352
