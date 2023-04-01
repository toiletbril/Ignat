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

import           Control.Monad.State
import           Data.List
import           Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Prelude             hiding (lookup)
import           System.Random
import           Util                (botLog)

-- TODO:  Store dictionary in a database instead of memory
-- TODO:  Separate dictionaries for servers

type Chain = Map Text [Text]

type Markov a = StateT Chain IO a

emptyChain :: Chain
emptyChain = M.empty

-- TODO: Add probability
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
      i <- liftIO $ randomRIO (0, length choices - 1)
      let w = choices !! i
      if n == 1
        then return w
        else do
          rest <- generate (n - 1) w
          return $ w <> " " <> rest

markovFromChain :: Chain -> Markov ()
markovFromChain = put

maxNum :: Int
maxNum = 88

markov :: Text -> Markov Text
markov text = do
  train $ T.words text
  len <- gets length
  botLog $
    "Markov chain updated with "
      <> show len
      <> " keys in dictionary."
  n <- liftIO $ randomRIO (10, maxNum)
  generate n $ last . T.words $ text

generateMarkov :: Chain -> Text -> Markov Text
generateMarkov c t = markovFromChain c *> markov t

runMarkov :: Markov a -> IO (a, Chain)
runMarkov m = runStateT m M.empty
