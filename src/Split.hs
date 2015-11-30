{- Split
By Gregory W. Schwartz

Collects the functions pertaining to splitting a fasta by the headers
(mainly genes)
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Split
    ( splitFasta
    ) where

-- Standard
import Data.List
import Control.Arrow
import Data.Function (on)

-- Cabal
import qualified Data.Text as T
import Data.Fasta.Text

-- | Split the list of fasta sequences by their header, assuming that the
-- header consist of only the gene
splitByHeader :: [FastaSequence] -> [[FastaSequence]]
splitByHeader = groupBy ((==) `on` fastaHeader)

-- | Annotate a list of fasta sequences by the header of the first sequence
annotateByHeader :: [FastaSequence] -> (T.Text, [FastaSequence])
annotateByHeader !xs = (header, xs)
  where
    header = fastaHeader . head $ xs

-- | Print the sequences for each grouping
printSequences :: [FastaSequence] -> T.Text
printSequences = T.intercalate "\n" . map showFasta

-- | Final splitting of fasta sequences into outputs where the first in the
-- tuple is the save file and the second is the content
splitFasta :: [FastaSequence] -> [(T.Text, T.Text)]
splitFasta = map (second printSequences . annotateByHeader) . splitByHeader
