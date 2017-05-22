import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Primes (checkIfNotExists, primeList)

main :: IO ()
main = hspec $ do
    describe "check if element not exists in list" $ do
        it "returns True if not exists" $ do
            checkIfNotExists 10 [1,2,3] `shouldBe` (True :: Bool)

        it "returns False when element exists" $
            checkIfNotExists 10 [1,2,10] `shouldBe` (False :: Bool)
    
    describe "check if there is a good sum of primes" $ do
        it "returns [5,3,2] when list of numbers is between 2 and 6" $ do
            primeList [2..6] `shouldBe` ([5,3,2] :: [Int])

