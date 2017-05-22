module Primes
    ( checkIfNotExists, primeList
    ) where

import Debug.Trace

checkIfNotExists::Int -> [Int] -> Bool
checkIfNotExists _ [] = True
checkIfNotExists value (x:xs)
    | x == value = False
    | x > value = True
    | xs == [] = True
    | otherwise = checkIfNotExists value xs

primeList::[Int] -> [Int]
primeList primeSeed =
    primeList' primeSeed []
    where 
        primeList'::[Int] -> [Int] -> [Int]
        primeList' (x:xs) primes = do
            let valueList = [x * y | y <- [2..((length(xs) `div` 2) + 1)]]
                deleteSub val lst = filter (`checkIfNotExists` lst) val
            primeList' (deleteSub xs valueList) [x] ++ primes
        primeList' [] primes = primes
