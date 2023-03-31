module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Discord
import Discord.Types
import Markov.Markov
import MessageHandler (handleMessage)
import UnliftIO (MVar, liftIO, newMVar)

main :: IO ()
main = do
  env <- readEnv
  state <- newMVar newState -- TODO: Read state from DB

  putStrLn "IgnatBot :: IO ()"
  putStrLn "IgnatBot = do"
  putStrLn "  Connecting to discord..."

  let token = head env
  err <-
    runDiscord $
      def
        { discordToken = token,
          discordOnStart = liftIO $ putStrLn "  Successfully connected.",
          discordOnEvent = eventHandler state,
          discordOnLog = T.putStrLn,
          discordOnEnd = return () -- TODO: Write state to DB
        }

  putStrLn "*** Encountered an unrecoverable error:"
  T.putStrLn err

eventHandler :: MVar MarkovState -> Event -> DiscordHandler ()
eventHandler state event = do
  case event of
    MessageCreate m -> handleMessage m state
    _ -> return ()

readEnv :: IO [T.Text]
readEnv = do
  file <- readFile ".env"
  return $ map T.pack $ lines file
