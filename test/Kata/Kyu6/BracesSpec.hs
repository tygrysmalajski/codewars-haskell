module Kata.Kyu6.BracesSpec where
import Kata.Kyu6.Braces
import Control.Monad (replicateM)
import Test.Hspec
import Test.QuickCheck

data Braced = Leaf | Paren [Braced] | Bracket [Braced] | Curly [Braced] deriving (Eq)

instance Show Braced where
  show Leaf        = ""
  show (Paren   a) = "(" ++ concatMap show a ++ ")"
  show (Bracket a) = "[" ++ concatMap show a ++ "]"
  show (Curly   a) = "{" ++ concatMap show a ++ "}"

instance Arbitrary Braced where
  arbitrary = sized arbBraced 
    where arbBraced n = do
            k <- choose (0,3) :: Gen Int
            l <- choose (0, min 5 n) -- maximum depth of 5
            let g = case k of
                  0 -> const Leaf
                  1 -> Paren
                  2 -> Bracket
                  3 -> Curly
            fmap g $ replicateM (n - l) $ arbBraced l

spec :: Spec
spec = do
    describe "validBraces" $ do
        it "should work for some examples" $ do
            validBraces "()"             `shouldBe` True
            validBraces "[([)"           `shouldBe` False
            validBraces "())({}}{()][][" `shouldBe` False
            validBraces "({})[({})]"     `shouldBe` True
        
        it "should work for random valid braces" $ property $ \xs -> 
            let xs' = show (xs :: Braced) in not (null xs') && (length xs' > 20) ==>
                validBraces xs' `shouldBe` True
        
        it "should work for random braces" $ property $
            forAll (listOf1 $ elements "()[]{}") $ \xs ->
                validBraces xs `shouldBe` solution xs
  where solution :: String -> Bool
        solution ""     = True
        solution (x:xs) = go [x] xs
          where go ('(':xs) (')':ys) = go xs ys
                go ('[':xs) (']':ys) = go xs ys
                go ('{':xs) ('}':ys) = go xs ys
                go xs       (y:ys)   = go (y:xs) ys
                go []       []       = True
                go _        []       = False