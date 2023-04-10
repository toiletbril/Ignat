{-# LANGUAGE OverloadedStrings #-}

module Markov
  ( Markov,
    Chain,
    shakespeareChain,
    emptyChain,
    generateMarkov,
    trainMarkov,
    runMarkov,
  )
where

import           Control.Monad.State
import           Data.List
import           Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Prelude             hiding (lookup)
import           System.Random
import           Util                (botLog, packShow, (<->))

import           System.IO           (IOMode (ReadMode), openFile)

-- TODO: Store dictionary in a database instead of memory
-- TODO: Separate dictionaries for servers

type Chain = Map Text (Int, Map Text Int)

type Markov a = StateT Chain IO a

emptyChain :: Chain
emptyChain = M.empty

-- DEBUG
shakespeareChain :: IO Chain
shakespeareChain = do
  f <- openFile "shakespeare.txt" ReadMode
  t <- T.hGetContents f
  (_, b) <- runMarkov $ train $ T.words t
  return b

addWord :: Text -> Text -> Markov ()
addWord k v = do
  chain <- get
  case chain !? k of
    Just (l, ws) ->
      case ws !? v of
        Just i -> do
          let i' = i + 1
          put $ M.insert k (max i' l, M.insert v i' ws) chain
        _ ->
          put $ M.insert k (max l 1, M.insert v 1 ws) chain
    _ ->
      put $ M.insert k (1, M.singleton v 1) chain

train :: [Text] -> Markov ()
train ws = do
  let pairs = zip ws (tail ws)
  mapM_ addWord' pairs
  len <- gets length
  botLog $
    "Markov chain updated with"
      <-> packShow len
      <-> "keys in dictionary."
  where
    addWord' = uncurry addWord

-- TODO: This causes the chain to repeat itself.
-- Also unefficient, since it's o(n)
prob :: Int -> Int -> Map Text Int -> Text
prob req i ws
  | i < length ws =
    let (word, count) = M.elemAt i ws in
      if count >= req
        then word
        else prob req (i + 1) ws
  | otherwise = error "prob() unreachable"

generate :: Int -> Text -> Markov Text
generate n word = do
  chain <- get
  case chain !? word of
    Nothing ->
      return T.empty
    Just (l, ws) -> do
      req <- liftIO $ randomRIO (0, l)
      let w = prob req 0 ws
      if n == 1
        then return $
          if T.last w == '.'
            then w
            else w <> "."
      else do
        rest <- generate (n - 1) w
        return $ w <-> rest

markovFromChain :: Chain -> Markov ()
markovFromChain = put

maxNum :: Int
maxNum = 88

markov :: Text -> Markov Text
markov text = do
  train $ T.words text
  n <- liftIO $ randomRIO (10, maxNum)
  generate n $ last . T.words $ text

generateMarkov :: Chain -> Text -> Markov Text
generateMarkov c t = markovFromChain c >> markov t

trainMarkov :: Chain -> Text -> Markov ()
trainMarkov c t = markovFromChain c >> train (T.words t)

runMarkov :: Markov a -> IO (a, Chain)
runMarkov m = runStateT m M.empty
