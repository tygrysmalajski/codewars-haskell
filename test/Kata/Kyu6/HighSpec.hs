module Kata.Kyu6.HighSpec where
import Kata.Kyu6.High
import Test.Hspec
import System.Random
import Data.List.Split
import Data.List

spec :: Spec
spec = do
    describe "Sample test" $ do
        it "man i need a taxi up to ubud" $ do
            high "man i need a taxi up to ubud" `shouldBe` "taxi"
        it "what time are we climbing up the volcano" $ do
            high "what time are we climbing up the volcano" `shouldBe` "volcano"
        it "take me to semynak" $ do
            high "take me to semynak" `shouldBe` "semynak"
        it "massage yes massage yes massage" $ do
            high "massage yes massage yes massage" `shouldBe` "massage"
        it "take two bintang and a dance please" $ do
            high "take two bintang and a dance please" `shouldBe` "bintang"
        it "Empty test" $ do
            high "" `shouldBe` ""  
        it "25 random tests" $ do
            ranTest 0

--local solution for random tests
highL :: String -> String
highL myS = splitS!!(maxInd!!0)
            where 
                splitS = splitOn " " myS
                valL = map sum (map (map aToO) splitS)
                maxInd = elemIndices (maximum valL) valL
aToO :: Char -> Int
aToO x  = (-96) + fromEnum x
-- random Test
ranTest :: Int -> IO()
ranTest n = 
    if n < 25 then do
        gen <- newStdGen
        p <- randomRIO (0,250)
        q <- randomRIO ('a','z')
        let strRnd = foldl (++) " "  (map (++ " ") (splitOn [q] (take p (randomRs('a','z') gen))))
        highL strRnd `shouldBe` high strRnd
        print ("Tested Random Value: " ++ strRnd)
        ranTest (n+1)
    else
        return ()