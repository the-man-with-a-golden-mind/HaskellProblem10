import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Primes (removeFromOrdList, sumPrimeList)

main :: IO ()
main = hspec $ do
    describe "check if there is a good sum of primes" $ do
        it "returns sum of all prime numbers when limit is 6" $ do
            sumPrimeList 6 `shouldBe` (10 :: Int)
