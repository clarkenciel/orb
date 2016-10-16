{-# language OverloadedStrings #-}

module Main where

import Markov
import Extract.PDF
import Control.Monad (guard)
import Data.Either (either)
import Data.Maybe (maybe)
import qualified Control.Monad.Random as Rand
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative (Parser, (<>), (<|>))
import qualified Options.Applicative as Opts

main :: IO ()
main = do
  cmd <- Opts.execParser $ Opts.info commands Opts.idm
  case cmd of
    PDFOptions input output -> stripPDF input output
    TrainOptions input output order -> buildMarkovModel order input output
    MarkovOptions input output len -> generateWithModel len input output

{- Main functions of this program -}

stripPDF :: FilePath -> FilePath -> IO ()
stripPDF input output =
  textFromPDF input
  >>= either print (TIO.writeFile output . T.concat)

buildMarkovModel :: MarkovOrder -> FilePath -> FilePath -> IO ()
buildMarkovModel order input output = do
  fileContents <- TIO.readFile input
  let mkv = freqToMarkov $ exampleToFreq order fileContents
  TIO.writeFile output (T.pack $ show mkv)

generateWithModel :: Int -> FilePath -> FilePath -> IO ()
generateWithModel maxWordCount mkvSource output = do
  mkv <- read . T.unpack <$> TIO.readFile mkvSource
  mkvSeed <- Rand.uniform $ Map.keys (table mkv)
  res <- T.concat  <$> generatePhrase maxWordCount mkv mkvSeed
  TIO.writeFile output res

{- Command line option parsing -}

data CommandOptions
  = PDFOptions
  { pdfInput :: FilePath
  , pdfOutput :: FilePath
  }
  | TrainOptions
  { trainInput :: FilePath
  , trainOutput :: FilePath
  , trainMarkovOrder :: MarkovOrder
  }
  | MarkovOptions
  { markovInput :: FilePath
  , markovOutput :: FilePath
  , markovPhraseLength :: Int
  }

commands =
  Opts.subparser $
  trainCommand
  <> markovCommand
  <> stripPdfCommand

trainCommand = Opts.command "train" $
  Opts.info trainOptions (Opts.progDesc "Train a markov model")
  
markovCommand = Opts.command "run-markov" $
  Opts.info markovOptions (Opts.progDesc "Run a markov model")

stripPdfCommand = Opts.command "strip-pdf" $
  Opts.info pdfOptions (Opts.progDesc "Extract the text from a pdf")


trainOptions = TrainOptions <$> input <*> output <*> Main.order

pdfOptions = PDFOptions <$> input <*> output

markovOptions = MarkovOptions <$> input <*> output <*> phraseLength

input = Opts.argument Opts.str $
  Opts.metavar "INPUT-FILE"

output = Opts.argument Opts.str $
  Opts.metavar "OUTPUT-FILE"

order = Opts.argument positiveReader $
  Opts.metavar "MARKOV-TABLE-ORDER"

phraseLength = Opts.argument positiveReader $
  Opts.metavar "GENERATED-TEXT-LENGTH"

positiveReader :: (Read a, Ord a, Num a) => Opts.ReadM a
positiveReader = (Opts.auto >>= greaterThan0) <|> (Opts.str >>= failure)
  where greaterThan0 x = x <$ guard (x > 0)
        failure s = Opts.readerError (s ++ " not greater than 0")
