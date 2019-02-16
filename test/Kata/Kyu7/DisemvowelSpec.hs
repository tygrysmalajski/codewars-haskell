module Kata.Kyu7.DisemvowelSpec where
import Kata.Kyu7.Disemvowel
import Test.Hspec

spec :: Spec
spec = do
    it "should work for single words" $ do
        disemvowel "hat" `shouldBe` "ht"
        disemvowel "queen" `shouldBe` "qn"
        disemvowel "onomatopoeia" `shouldBe` "nmtp"
    it "should work for phrases" $ do
        disemvowel "i like toast" `shouldBe` " lk tst"
        disemvowel "the quick brown fox jumps over the lazy dog" `shouldBe` "th qck brwn fx jmps vr th lzy dg"
    it "should preserve case" $ do
        disemvowel "The Quick Brown Fox Jumps Over The Lazy Dog" `shouldBe` "Th Qck Brwn Fx Jmps vr Th Lzy Dg"
    it "should preserve punctuation" $ do
        disemvowel "This. Is, A? Misuse; Of: Punctuation!" `shouldBe` "Ths. s, ? Mss; f: Pncttn!"