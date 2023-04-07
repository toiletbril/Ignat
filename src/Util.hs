{-# LANGUAGE OverloadedStrings #-}

module Util where

import           App
import           Control.Monad
import           Data.Maybe    (fromMaybe)
import           Data.Text     as T
import           Data.Text.IO  as T (putStrLn)
import           Discord.Types
import           UnliftIO      (MonadIO, liftIO)

botLog :: MonadIO m => Text -> m ()
botLog s = liftIO $ T.putStrLn $ "> " <> s

packShow :: Show a => a -> Text
packShow = T.pack . show

commandType :: Text -> Text
commandType x = fromMaybe "" $ t <=< headMay $ T.words x
  where
    t "" = Nothing
    t a  = Just (T.tail a)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay a  = Just (Prelude.head a)

startsWith :: Message -> Text -> Bool
startsWith = isPrefixOf . messageContent

withoutFirst :: Text -> Text
withoutFirst s = T.unwords . Prelude.tail $ T.words s

isUser :: Message -> Bool
isUser = not . userIsBot . messageAuthor

isMarkovResponse :: Message -> Bool
isMarkovResponse m = Prelude.any (\x -> userId x == botId) (messageMentions m) && isUser m

isValidMessage :: Message -> Bool
isValidMessage m = m `startsWith` botPrefix && isUser m
