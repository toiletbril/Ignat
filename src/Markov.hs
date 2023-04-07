{-# LANGUAGE OverloadedStrings #-}

module Markov
  -- ( Markov,
  --   Chain,
  --   emptyChain,
  --   generateMarkov,
  --   runMarkov,
  -- )
where

import           Control.Monad.State
import           Data.List
import           Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Prelude             hiding (lookup)
import           System.IO           (IOMode (ReadMode), openFile)
import           System.Random
import           Util                (botLog)


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
  (_, b) <- runMarkov $ markov t
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
  where
    addWord' = uncurry addWord

-- TODO: This is weird and causes the chain to repeat itself.
prob :: Int -> Int -> Map Text Int -> Text
prob req i m
  | i < length m =
    let (word, count) = M.elemAt i m in
      if count >= req
        then word
        else prob req (i + 1) m
  | otherwise = error "unreachable"

generate :: Int -> Text -> Markov Text
generate n word = do
  chain <- get
  case chain !? word of
    Nothing ->
      return T.empty
    Just (l, ws) -> do
      req <- liftIO $ randomRIO (0, l - 1)
      let w = prob req 0 ws
      if n == 1
        then return $
          if T.last w == '.'
            then w
            else w <> "."
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
      <> T.pack (show len)
      <> " keys in dictionary."
  n <- liftIO $ randomRIO (10, maxNum)
  generate n $ last . T.words $ text

generateMarkov :: Chain -> Text -> Markov Text
generateMarkov c t = markovFromChain c *> markov t

runMarkov :: Markov a -> IO (a, Chain)
runMarkov m = runStateT m M.empty
