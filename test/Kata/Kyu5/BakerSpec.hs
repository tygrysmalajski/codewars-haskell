module Kata.Kyu5.BakerSpec where
import Kata.Kyu5.Baker
import Data.List (nubBy)
import Data.Function (on)
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

spec :: Spec
spec = do
    describe "cakes" $ do
        it "should work for some small examples" $ do
            cakes [("flour",500), ("sugar",200), ("eggs",1)] [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)]  `shouldBe` 2
            cakes [("apples",3), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)] [("sugar",500), ("flour",2000), ("milk",2000)] `shouldBe` 0
        
        it "should work for basic recipes" $ do
            cakes [("flour",500), ("sugar",200), ("eggs",1)] [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)] `shouldBe` 2
            cakes [("cream",200), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)] [("sugar",1700), ("flour",20000), ("milk",20000), ("oil",30000), ("cream",5000)] `shouldBe` 11
        
        it "should work for missing ingredients" $
            cakes [("apples",3), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)] [("sugar",500), ("flour",2000), ("milk",2000)] `shouldBe` 0
        
        it "should work if not enough ingredients are available" $ do
            cakes [("apples",3), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)] [("sugar",500), ("flour",2000), ("milk",2000), ("apples",15), ("oil",20)] `shouldBe` 0
        
        it "should work if we've been robbed" $ do
            cakes [("eggs",4), ("flour",400)] [] `shouldBe` 0
        
        it "should work for shuffled lists" $ do 
            cakes [("cream",1), ("flour",3), ("sugar",1), ("milk",1), ("oil",1), ("eggs",1)] [("sugar",1), ("eggs",1), ("flour",3), ("cream",1), ("oil",1), ("milk",1)] `shouldBe` 1
        it "should work for random recipes" $ do
            let ingredients = ["flour", "eggs", "oil", "milk", "apples", "sugar", "cream", "pears", "butter", "nuts", "chocolate", "cocoa", "crumbles"]
            let positive    = arbitrary `suchThat` (> 0)
            property $ 
                forAll (zip <$> (listOf1 $ elements ingredients) <*> listOf1 positive) $ \recipe' ->
                forAll (zip <$> (listOf  $ elements ingredients) <*> listOf  positive) $ \storage' ->
                let recipe  = nubBy ((==) `on` fst) recipe'
                    storage = nubBy ((==) `on` fst) storage'
                in cakes recipe storage `shouldBe` solution recipe storage
    
  where
    solution :: [(String, Int)] -> [(String, Int)] -> Int
    solution r s = minimum $ map (\(i,a) -> maybe 0 (`div` a) $ lookup i s) r