module App where

import           Control.Monad.State    hiding (withState)
import           Data.Map               as M
import           Data.Text              as T
import           Data.Text.IO           as T
import           Discord.Internal.Types (UserId)

type Chain = Map Text [Text]

type Markov a = StateT Chain IO a

newtype MarkovState = State
  { getChain :: Chain
  }

data App = App { markovState :: MarkovState, env :: [Text] }

readEnv :: IO [Text]
readEnv = do
  file <- T.readFile ".env"
  return $ T.lines file

-- TODO: Read these from .env
botPrefix :: Char
botPrefix = '%'

botId :: UserId
botId = 1042440644028813352
