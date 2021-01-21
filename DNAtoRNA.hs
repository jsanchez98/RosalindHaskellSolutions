module DNAtoRNA where

import System.IO

main :: IO ()
main = do input <- readFile "rosalind_rna.txt"
          output <- openFile "rna.txt" WriteMode
          hPutStr output $ map convertDNA input
          hClose output
          putStr "done"

convertDNA :: Char -> Char
convertDNA c = case c of 'T' -> 'U'
                         _   -> c
