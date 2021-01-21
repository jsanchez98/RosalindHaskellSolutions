module GCPercent where

import Data.List
import Debug.Trace
import Data.Ord

main :: IO ()
main = do input <- readFile "rosalind_gc.txt"
          putStrLn $ show $ calcHighestGC input
          putStr "done"

calcHighestGC :: String -> (Maybe Double, String)
calcHighestGC input = trace ("tuples" ++ show tuples) $ maximumBy (comparing fst) $ tuples
                      where tuples = zip (map (calcGC input) $ names) names
                            names = filter (elem '>') file
                            file = lines input

calcGC :: String -> String -> Maybe Double
calcGC file iD = do dna <- getDNA iD file
                    return $ 100 * ((fromIntegral $ foldl countGC 0 dna) / (fromIntegral $ length dna))

countGC :: Int -> Char -> Int
countGC i c = case c of 'G' -> i+1
                        'C' -> i+1
                        _   -> i

getDNA :: String -> String -> Maybe String
getDNA i ss = do x <- findIndex (isSubsequenceOf i) file
                 return $ concat $ takeWhile (notElem '>') $ drop (x+1) file
                 where file = lines ss
