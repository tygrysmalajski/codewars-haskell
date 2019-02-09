module Kata.Kyu6.DigPowSpec where
import Kata.Kyu6.DigPow
import Test.Hspec

spec :: Spec
spec = do
    describe "playDigits" $ do
        it "1st series" $ do
            digpow 89 1 `shouldBe` 1
            digpow 92 1 `shouldBe` -1
            digpow 46288 3 `shouldBe` 51
            digpow 114 3 `shouldBe` 9
            digpow 46288 5 `shouldBe` -1
        it "2nd series" $ do
            digpow 135 1 `shouldBe` 1
            digpow 175 1 `shouldBe` 1
            digpow 518 1 `shouldBe` 1
            digpow 598 1 `shouldBe` 1
            digpow 1306 1 `shouldBe` 1
        it "3rd series" $ do
            digpow 2427 1 `shouldBe` 1
            digpow 2646798 1 `shouldBe` 1
            digpow 3456789 1 `shouldBe` -1
            digpow 3456789 5 `shouldBe` -1
            digpow 198 1 `shouldBe` 3
        it "4th series" $ do
            digpow 249 1 `shouldBe` 3
            digpow 1377 1 `shouldBe` 2
            digpow 1676 1 `shouldBe` 1
            digpow 695 2 `shouldBe` 2
            digpow 1878 2 `shouldBe` 19
        it "5th series" $ do
            digpow 7388 2 `shouldBe` 5
            digpow 47016 2 `shouldBe` 1
            digpow 542186 2 `shouldBe` 1
            digpow 261 3 `shouldBe` 5
            digpow 1385 3 `shouldBe` 35
        it "6th series" $ do
            digpow 2697 3 `shouldBe` 66
            digpow 6376 3 `shouldBe` 10
            digpow 6714 3 `shouldBe` 1
            digpow 63760 3 `shouldBe` 1
            digpow 63761 3 `shouldBe` 1
            digpow 132921 3 `shouldBe` 4
            digpow 10383 6 `shouldBe` 12933