{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Maybe    (fromMaybe)
import           Data.String   (IsString)
import           Data.Text     as T
import           Data.Text.IO  as T (putStrLn)
import           Discord.Types
import           UnliftIO      (MonadIO, liftIO)

botLog :: MonadIO m => Text -> m ()
botLog s = liftIO $ T.putStrLn $ ">" <-> s

packShow :: Show a => a -> Text
packShow = T.pack . show

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay a  = Just (Prelude.head a)

tTailMay :: Text -> Maybe Text
tTailMay "" = Nothing
tTailMay a  = Just (T.tail a)

tHeadMay :: Text -> Maybe Text
tHeadMay "" = Nothing
tHeadMay a  = Just (singleton $ T.head a)

(<->) :: (IsString a, Semigroup a) => a -> a -> a
(<->) = (<>) . flip (<>) " "

startsWith :: Message -> Text -> Bool
startsWith = isPrefixOf . messageContent

withoutFirst :: Text -> Text
withoutFirst s = T.unwords . Prelude.tail $ T.words s

capFirst :: Text -> Text
capFirst t = fromMaybe "" $ (toUpper <$> x) <> xs
  where x = tHeadMay t
        xs = tTailMay t

isUser :: Message -> Bool
isUser = not . userIsBot . messageAuthor
