module CountNTs where

import System.IO

main :: IO ()
main = do input <- readFile "rosalind_dna.txt"
          output <- openFile "count.txt" WriteMode
          hPutStr output $ countATCG input
          hClose output
          putStr "done"

countATCG :: String -> String
countATCG ss = show (count !! 0) ++ " " ++ show (count !! 1) ++ " " ++ show (count !! 2) ++ " " ++show (count !! 3)
              where count = foldl updateCount [0,0,0,0] ss

updateCount :: [Int] -> Char -> [Int]
updateCount (a:c:g:t:_) s = case s of 'A' -> [a+1,c,g,t]
                                      'T' -> [a,c,g,t+1]
                                      'C' -> [a,c+1,g,t]
                                      'G' -> [a,c,g+1,t]
                                      _  -> [a,c,g,t]
