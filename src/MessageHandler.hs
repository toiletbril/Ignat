{-# LANGUAGE OverloadedStrings #-}

module MessageHandler where

import           Discord       (DiscordHandler)
import           Discord.Types (Message (messageContent))
import           Markov.Markov
import           UnliftIO
import           Util

-- TODO: Make this cooler
handleMessage :: Message -> MVar MarkovState -> DiscordHandler ()
handleMessage m s
  | isValidMessage m =
      case commandType contents of
        "haskell" -> reply m "This is the message"
        _         -> return ()
  | isMarkovResponse m = do
      chain <- getChain <$> takeMVar s
      (generated, chain') <- liftIO . runMarkov $ generateMarkov chain contents
      reply m $ withoutFirst contents <> " " <> generated
      putMVar s $ withState chain'
  | otherwise = do
      chain <- getChain <$> takeMVar s
      (_, chain') <- liftIO . runMarkov $ generateMarkov chain contents
      putMVar s $ withState chain'
  where
    contents = messageContent m
