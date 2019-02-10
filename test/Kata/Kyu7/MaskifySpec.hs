module Kata.Kyu7.MaskifySpec where
import Kata.Kyu7.Maskify
import Test.Hspec
import Test.QuickCheck
    
spec :: Spec
spec = do
    describe "maskify" $ do
        it "should mask the credit card"    $ maskify "4556364607935616" `shouldBe` "############5616"
        it "should mask another number"     $ maskify "64607935616" `shouldBe` "#######5616"
        it "should mask a short number"     $ maskify "616" `shouldBe` "616"
        it "should mask a single character" $ maskify "1" `shouldBe` "1"
        it "should mask an empty string"    $ maskify "" `shouldBe` ""
        it "should mask your pet"           $ maskify "Skippy" `shouldBe` "##ippy"
        it "should mask batman"             $ 
          maskify "Nananananananananananananananana Batman!" `shouldBe` "####################################man!"
        it "shouldn't change the length" $ 
          property $ \x -> 
            length x == length (maskify x)
        it "shouldn't change the last four" $
          property $ \x ->
            take 4 (reverse x) == take 4 (reverse $ maskify x)
        it "should change all others to '#'" $
          property $ \x ->
            all (=='#') . drop 4 . reverse . maskify $ x