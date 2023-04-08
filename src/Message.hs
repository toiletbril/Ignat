{-# LANGUAGE OverloadedStrings #-}

module Message where

import           App              (botId, botPrefix)
import           Control.Monad    (when, (<=<))
import           Data.Maybe       (fromMaybe)
import           Data.Text        as T
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
      reply m $ capFirst $ withoutFirst contents <-> generated
      putMVar s chain'
  | otherwise = when (isUser m) $ do
      chain <- takeMVar s
      (_, chain') <- liftIO . runMarkov $ generateMarkov chain contents
      putMVar s chain'
  where
    contents = messageContent m

commandType :: Text -> Text
commandType x = fromMaybe "" $ tTailMay <=< headMay $ T.words x

isMarkovResponse :: Message -> Bool
isMarkovResponse m = Prelude.any (\x -> userId x == botId) (messageMentions m) && isUser m

isValidMessage :: Message -> Bool
isValidMessage m = m `startsWith` botPrefix && isUser m

reply :: Message -> Text -> DiscordHandler ()
reply m s = do
  botLog $
    "Replying on a message:\n"
      <> packShow (userName $ messageAuthor m)
      <> ":" <-> messageContent m
      <> "\n> with:\n" <> s
  _ <- restCall $ R.CreateMessage (messageChannelId m) s
  return ()
