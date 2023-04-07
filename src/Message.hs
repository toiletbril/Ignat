{-# LANGUAGE OverloadedStrings #-}

module Message where

import           Data.Text        (Text)
import           Discord          (DiscordHandler, restCall)
import           Discord.Requests as R
import           Discord.Types
import           Markov
import           UnliftIO
import           Util

-- TODO: Make this cooler
handleMessage :: Message -> MVar Chain -> DiscordHandler ()
handleMessage m s
  | isValidMessage m =
      case commandType contents of
        "haskell" -> reply m "Hello World"
        _         -> return ()
  | isMarkovResponse m = do
      chain <- takeMVar s
      (generated, chain') <- liftIO . runMarkov $ generateMarkov chain contents
      reply m $ withoutFirst contents <> " " <> generated
      putMVar s chain'
  | otherwise = do
      chain <- takeMVar s
      (_, chain') <- liftIO . runMarkov $ generateMarkov chain contents
      putMVar s chain'
  where
    contents = messageContent m

reply :: Message -> Text -> DiscordHandler ()
reply m s = do
  botLog $
    "Replying on a message:\n"
      <> "[" <> packShow (messageTimestamp m) <> "] "
      <> packShow (userName $ messageAuthor m) <> ": "
      <> messageContent m
      <> "\nwith:\n" <> s
  _ <- restCall $ R.CreateMessage (messageChannelId m) s
  return ()
