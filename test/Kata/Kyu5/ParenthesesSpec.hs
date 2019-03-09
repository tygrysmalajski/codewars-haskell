module Kata.Kyu5.ParenthesesSpec where
import Kata.Kyu5.Parentheses (validParentheses)
import Test.Hspec
import Test.QuickCheck
import Control.Monad (guard, foldM)

spec :: Spec
spec = do
    describe "validParentheses" $ do
        it "should work for some examples" $ do
            validParentheses "()" `shouldBe` True
            validParentheses ")(" `shouldBe` False
            validParentheses ")"  `shouldBe` False
            validParentheses ""  `shouldBe` True
            validParentheses "(())((()())())"  `shouldBe` True
          
        it "should work for random inputs" $ do
            property $ forAll (listOf1 $ elements "()") $ \xs ->
                validParentheses xs `shouldBe` solution xs
    where solution = maybe False (==0) . foldM go 0
            where 
                go x '(' = return . succ $ x
                go x ')' = guard (x > 0) >> (return . pred $ x)