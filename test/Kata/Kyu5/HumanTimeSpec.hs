module Kata.Kyu5.HumanTimeSpec where
import Kata.Kyu5.HumanTime (humanReadable)
import Data.List (intercalate)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "humanReadable" $ do
        it "should work for some examples" $ do
            humanReadable 0      `shouldBe` "00:00:00"
            humanReadable 59     `shouldBe` "00:00:59"
            humanReadable 60     `shouldBe` "00:01:00"
            humanReadable 90     `shouldBe` "00:01:30"
            humanReadable 86399  `shouldBe` "23:59:59"
            humanReadable 359999 `shouldBe` "99:59:59"
        it "should work for random ints" $ do
            property $ forAll (elements [0..359999]) $ \x ->
                humanReadable x `shouldBe` solution x
    where 
        solution x = intercalate ":" . map (f . show) $ [h,m,s]
            where 
                f [x]  = '0':[x]
                f x    =  x
                (h,x') = quotRem x  3600
                (m,s)  = quotRem x' 60