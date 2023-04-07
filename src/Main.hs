module Main where

import           App
import qualified Data.Text.IO  as T
import           Discord
import           Discord.Types
import           Markov
import           Message       (handleMessage)
import           UnliftIO      (MVar, liftIO, newMVar)

-- TODO: Add debug flag
main :: IO ()
main = do
  env <- readEnv

  shakespeare <- shakespeareChain
  -- TODO: DB
  state <- newMVar shakespeare

  putStrLn "IgnatBot :: IO ()"
  putStrLn "IgnatBot = do"
  putStrLn "  Connecting to discord..."

  let token = head env
  err <-
    runDiscord $
      def
        { discordToken = token,
          discordOnStart = liftIO $ putStrLn "  Successfully connected. Logs will appear below.",
          discordOnEvent = eventHandler state,
          discordOnLog = T.putStrLn,
          discordOnEnd = return ()
        }

  putStrLn "*** Encountered an unrecoverable error:"
  T.putStrLn err

eventHandler :: MVar Chain -> Event -> DiscordHandler ()
eventHandler state event = do
  case event of
    MessageCreate m -> handleMessage m state
    _               -> return ()
