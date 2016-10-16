{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language OverloadedStrings #-}

module Markov where

import Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Rand
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (maybe)
import Control.Monad ((>=>))
import Data.List (foldl1', intersperse)

{- Data types -}
type MarkovOrder = Int
type Path a = [a]
data Markov a b
  = M { order :: MarkovOrder
      , table :: Map (Path a) (Map a b)
      }
  deriving (Show, Read, Functor)

type TextPath = Path Text
type WordFrequencyTable = Markov Text Integer
type MarkovTable = Markov Text Rational
type MarkovRow = [(Text, Rational)]

{- Building Markov tables -}
exampleToMarkov = fmap freqToMarkov . exampleToFreq

exampleToFreq :: MarkovOrder -> Text -> WordFrequencyTable
exampleToFreq order example =
  let segments = segment order $ T.words example
  in foldl1' joinTables $ buildTables segments
  where buildTables = map (uncurry wordFreqTable)
        joinTables t1 t2 =
          M { order = order
            , table = Map.unionWith (Map.unionWith (+)) (table t1) (table t2)
            }

freqToMarkov :: WordFrequencyTable -> MarkovTable
freqToMarkov wft@M{table} =
  wft { table = fmap calcProb table }
  where calcProb m =
          let mSum = fromInteger $ sum m
          in fmap ((/mSum) . fromInteger) m

segment n xs = [(init ys, last ys) |
                 ys <- zipWith ($) fs (repeat $ xs ++ [head xs]),
                 not (null ys) && length ys >= n ]
  where fs = fmap (take n) . drop <$> [0..length xs]

empty = Map.empty

wordFreqTable :: [Text] -> Text -> WordFrequencyTable
wordFreqTable lead last =
  M (length lead) (Map.singleton lead $ Map.singleton last 1)

addEntry :: TextPath -> Text -> WordFrequencyTable -> WordFrequencyTable
addEntry lead last wft@M{table} =
  wft { table = Map.update (return . Map.insertWith (+) last 1) lead
                $ table
      }

{- Generating text from a markov table -}

generatePhrase :: MonadRandom m => Int -> MarkovTable -> [Text] -> m [Text]
generatePhrase phraseLength mkv seed =
  go phraseLength seed (intersperse " " seed)
  where go 0 _ acc = return acc
        go n seed acc = do
          next <- nextWord seed mkv
          maybe (return acc) (continue n seed acc) next
        continue n seed acc next =
          go (n-1) (drop 1 $ seed ++ [next]) (acc ++ [" "] ++ [next])

nextWord :: MonadRandom m => TextPath -> MarkovTable -> m (Maybe Text)
nextWord path mkv =
  maybe (pure Nothing) (fmap Just . Rand.fromList) (fetch path mkv)

fetch :: TextPath -> MarkovTable -> Maybe MarkovRow
fetch path M{table} = Map.toList <$> Map.lookup path table
