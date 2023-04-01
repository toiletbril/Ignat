{-# LANGUAGE OverloadedStrings #-}

module Message where

import           Discord       (DiscordHandler)
import           Discord.Types (Message (messageContent))
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
