module Main where

import Primes (primeList)

main :: IO ()
main = do
    let primeSeed = [2..2000000]
    print (foldr (+) 0 ([1] ++ (primeList primeSeed)))