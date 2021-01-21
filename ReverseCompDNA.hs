module ReverseCompDNA where

import System.IO

main :: IO ()
main = do input <- readFile "rosalind_revc.txt"
          output <- openFile "rna.txt" WriteMode
          hPutStr output $ reverseComp input
          hClose output
          putStr "done"

reverseComp :: String -> String
reverseComp [] = []
reverseComp (s:ss) = case s of 'A' -> reverseComp ss ++ ['T']
                               'T' -> reverseComp ss ++ ['A']
                               'C' -> reverseComp ss ++ ['G']
                               'G' -> reverseComp ss ++ ['C']
                               x   -> reverseComp ss ++ [x]
