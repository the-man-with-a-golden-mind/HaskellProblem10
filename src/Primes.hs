module Primes
    ( sumPrimeList, removeFromOrdList
    ) where

import           Debug.Trace

removeFromOrdList::[Int] -> [Int] -> [Int]
removeFromOrdList toRemove fromRemove =
  removeFromOrdList' toRemove fromRemove []
    where
      removeFromOrdList'::[Int] -> [Int] -> [Int] -> [Int]
      removeFromOrdList' t_all@(t_x: t_xs) f_all@(f_x: f_xs) prePrimes
        | null f_xs = prePrimes
        | null t_xs = prePrimes
        | t_x == f_x = removeFromOrdList' t_xs f_xs prePrimes
        | t_x > f_x = removeFromOrdList' t_all f_xs (prePrimes ++ [f_x])
        | t_x < f_x = removeFromOrdList' t_xs f_all prePrimes

sumPrimeList::Int -> Int
sumPrimeList limit =
    sum $ primeList' [2..limit] []
    where
        primeList'::[Int] -> [Int] -> [Int]
        primeList' x_all@(x:xs) primes
          | x^2 > limit = primes ++ x_all
          | otherwise = do
            let valueList = [y * x | y <- [x..((limit `div` x) + 1)]]
            primeList' (removeFromOrdList valueList xs) [x] ++ primes
        primeList' [] primes = primes
