module Util where

import Control.Monad
import Data.Text as T
import Discord
import Discord.Requests as R
import Discord.Types
import UnliftIO (MonadIO, liftIO)

-- TODO: Read bot prefix from .env
botPrefix :: Char
botPrefix = '%'

botId :: UserId
botId = 1042440644028813352

botLog :: MonadIO m => String -> m ()
botLog s = liftIO $ putStrLn $ "> " <> s

commandType :: Text -> Text
commandType = T.tail . Prelude.head . T.words

startsWith :: Message -> Char -> Bool
startsWith m = (==) . T.head $ messageContent m

withoutFirst :: Text -> Text
withoutFirst s = T.unwords . Prelude.tail $ T.words s

isUser :: Message -> Bool
isUser = not . userIsBot . messageAuthor

isMarkovResponse :: Message -> Bool
isMarkovResponse m = Prelude.any (\x -> userId x == botId) (messageMentions m) && isUser m

isValidMessage :: Message -> Bool
isValidMessage m = m `startsWith` botPrefix && isUser m

reply :: Message -> Text -> DiscordHandler ()
reply m s = do
  botLog $
    "Replying on a message:\n"
      <> "["
      <> show (messageTimestamp m)
      <> "] "
      <> show (userName $ messageAuthor m)
      <> ": "
      <> unpack (messageContent m)
      <> "\nwith:\n"
      <> unpack s
  void . restCall $ R.CreateMessage (messageChannelId m) s
