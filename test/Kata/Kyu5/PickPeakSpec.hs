module Kata.Kyu5.PickPeakSpec where
import Kata.Kyu5.PickPeak
import Test.Hspec

spec :: Spec
spec = do
    describe "Some harder test cases" $ do
        it "[3,2,3,6,4,1,2,3,2,1,2,3]" $ do
            let myPeak = PickedPeaks {pos = [3,7], peaks = [6,3]}
            pickPeaks [3,2,3,6,4,1,2,3,2,1,2,3] `shouldBe` myPeak
        it "should support finding peaks, but should ignore peaks on the edge of the array" $ do
            pickPeaks [3,2,3,6,4,1,2,3,2,1,2,3] `shouldBe` PickedPeaks {pos = [3,7], peaks = [6,3]}
        it "should support finding peaks; if the peak is a plateau, it should only return the position of the first element of the plateau" $ do
            pickPeaks [3,2,3,6,4,1,2,3,2,1,2,2,2,1] `shouldBe` PickedPeaks {pos = [3,7,10], peaks = [6,3,2]}
        it "should support finding peaks; if the peak is a plateau, it should only return the position of the first element of the plateau" $ do
            pickPeaks [2,1,3,1,2,2,2,2,1] `shouldBe` PickedPeaks {pos = [2,4], peaks = [3,2]}
        it "should support finding peaks, but should ignore peaks on the edge of the array" $ do
            pickPeaks [2,1,3,1,2,2,2,2] `shouldBe` PickedPeaks {pos = [2], peaks = [3]}
        it "should support finding peaks, but should ignore peaks on the edge of the array" $ do
            pickPeaks [2,1,3,2,2,2,2,5,6] `shouldBe` PickedPeaks {pos = [2], peaks = [3]}
        it "should support finding peaks, despite the plateau" $ do
            pickPeaks [2,1,3,2,2,2,2,1] `shouldBe` PickedPeaks {pos = [2], peaks = [3]} 
        it "should support finding peaks" $ do
            pickPeaks [1,2,5,4,3,2,3,6,4,1,2,3,3,4,5,3,2,1,2,3,5,5,4,3] `shouldBe` PickedPeaks {pos = [2,7,14,20], peaks = [5,6,5,5]}
        it "should return a PickedPeaks withs empty lists on empty input" $ do
            pickPeaks [] `shouldBe` PickedPeaks {pos = [], peaks = []}
        it "should cope with a small mountain range" $ do
            pickPeaks [1,1] `shouldBe` PickedPeaks {pos = [], peaks = []}
        it "should cope with a flat area" $ do
            pickPeaks [1,1,1,1] `shouldBe` PickedPeaks {pos = [], peaks = []}
        it "should cope with a small range" $ do
            pickPeaks [11] `shouldBe` PickedPeaks {pos = [], peaks = []}