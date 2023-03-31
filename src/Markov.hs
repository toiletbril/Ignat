{-# LANGUAGE OverloadedStrings #-}

module Markov
  ( Markov,
    Chain,
    emptyChain,
    markovFromChain,
    generateMarkov,
    runMarkov,
  )
where

import           App
import           Control.Monad.State hiding (withState)
import           Data.List
import           Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Prelude             hiding (lookup)
import           System.Random
import           Util                (botLog)

-- TODO:  Store dictionary in a database instead of memory
-- TODO:  Separate dictionaries for servers

emptyChain :: Chain
emptyChain = M.empty

-- TODO: Add probability?
addWord :: Text -> Text -> Markov ()
addWord k v = do
  chain <- get
  let ws = findWithDefault [] k chain
  case elemIndex v ws of
    Just _ -> return ()
    _      -> put $ M.insert k (v : ws) chain

train :: [Text] -> Markov ()
train ws = do
  let pairs = zip ws (tail ws)
  mapM_ addWord' pairs
  where
    addWord' = uncurry addWord

generate :: Int -> Text -> Markov Text
generate n word = do
  chain <- get
  let choices = findWithDefault [] word chain
  case choices of
    [] ->
      return T.empty
    _ -> do
      nxt <- liftIO $ randomRIO (0, length choices - 1)
      let nextWord = choices !! nxt
      if n == 1
        then return nextWord
        else do
          rest <- generate (n - 1) nextWord
          return $ nextWord <> " " <> rest

markovFromChain :: Chain -> Markov ()
markovFromChain = put

maxNum :: Int
maxNum = 88

markov :: Text -> Markov Text
markov text = do
  train $ T.words text
  chain <- get
  botLog $
    "Markov chain updated with "
      <> show (length chain)
      <> " keys in dictionary."
  let inputs = T.words text
  let lastWord = last inputs
  n <- liftIO $ randomRIO (10, maxNum)
  generate n lastWord

generateMarkov :: Chain -> Text -> Markov Text
generateMarkov c t = markovFromChain c *> markov t

runMarkov :: Markov a -> IO (a, Chain)
runMarkov m = runStateT m M.empty
