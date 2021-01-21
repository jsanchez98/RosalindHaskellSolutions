module RecurRelation where

firstfib :: Int -> Int -> Int
firstfib 1 k = 1
firstfib 2 k = 1
firstfib n k = firstfib (n-1) k + k * firstfib (n-2) k

fib :: Int -> Int -> Int
fib k n = (xs !! (n-1)) + k*(xs !! (n-2))
          where xs = 1:1:map (fib k) [2..]

fastFib :: Int -> Int -> Int
fastFib n k = last $ take n $ fib1 k

fib1 :: Int -> [Int]
fib1 k = zipWith (\x y -> x + k*y) (1:(1:fib1 k)) (1:fib1 k)

nfib = zipWith (+) (0:(1:nfib)) (1:nfib)
