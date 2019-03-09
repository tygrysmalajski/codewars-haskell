module Kata.Kyu6.SmileSpec where
import Kata.Kyu6.Smile
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
    it "Handles basic tests" $ do
        countSmileys []                               `shouldBe` 0
        countSmileys [":D",":~)",";~D",":)"]          `shouldBe` 4
        countSmileys [":)",":(",":D",":O",":;"]       `shouldBe` 2
        countSmileys [";]", ":[", ";*", ":$", ";-D"]  `shouldBe` 1
        countSmileys [";", ")", ";*", ":$", "8-D"]    `shouldBe` 0
    modifyMaxSuccess (const 1000) $ it "Handles randoms as well" $ property $  
        forAll genArg $ \s -> do
            -- print s
            countSmileys s `shouldBe` sol s

genArg :: Gen [String]
genArg = do
    l   <- choose (2,50)
    vectorOf l (oneof [genSmile, genSmile >>= shuffle,resize 3 arbitrary])
    
genSmile :: Gen String
genSmile = elements [":-)", ";-)",  ":~)", ";~)",  ":)", ";)",   ":-D", ";-D",  ":~D", ";~D",  ":D", ";D"]

sol :: [String] -> Int
sol = length . filter go 
    where
    go ":-)"  = True
    go ";-)"  = True
    go ":~)"  = True
    go ";~)"  = True
    go ":)"   = True
    go ";)"   = True
    go ":-D"  = True
    go ";-D"  = True
    go ":~D"  = True
    go ";~D"  = True
    go ":D"   = True
    go ";D"   = True
    go _      = False