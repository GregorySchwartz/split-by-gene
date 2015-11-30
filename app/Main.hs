{- split-by-gene
By Gregory W. Schwartz

Splits a fasta file up into many fasta files, one per header (assuming that
the header consists of what to split by)
-}

{-# LANGUAGE BangPatterns #-}

module Main where

-- Standard
import qualified System.IO as IO
import Control.Monad

-- Cabal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Fasta.Text
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Text.IO as PT

-- Local
import Split

-- Command line arguments
data Options = Options { input                    :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The input fasta file. If nothing, then stdin" )

splitByGene :: Options -> IO ()
splitByGene opts = do
    hIn  <- if null . input $ opts
                then return IO.stdin
                else IO.openFile (input opts) IO.ReadMode

    fastaList <- runEffect $ P.toListM $ pipesFasta (PT.fromHandle hIn)

    let splitOutput = splitFasta fastaList

    forM_ splitOutput $ \(!filename, !output) ->
        T.writeFile (T.unpack filename ++ ".fasta") output

main :: IO ()
main = execParser opts >>= splitByGene
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Split a fasta file into many fasta files, one per each header\
                 \ in the file. Assumes that the file is pre-sorted by header."
     <> header "split-by-gene, Gregory W. Schwartz" )
