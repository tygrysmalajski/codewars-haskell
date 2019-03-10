module Kata.Kyu4.CodewarsRankingSpec where
import Kata.Kyu4.CodewarsRanking(newUser, rank, progress, incProgress)
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Test.HUnit

data Result = R Int Int deriving (Eq)
instance Show Result where
    show (R r p) = concat ["rank=", show r, " progress=", show p]

doTest userBefore trank exRank exProgress = do
    let userAfter = incProgress trank userBefore
        expected = R exRank exProgress
        before = R (rank userBefore) (progress userBefore)
        actual = R (rank userAfter) (progress userAfter)
        errorMsg = unlines [ "User: " ++ show before
                            , "Task: rank=" ++ show trank
                            ]
    assertEqual errorMsg expected actual
    return userAfter
    
spec :: Spec
spec = do 
    describe "User" $ do
        it "should start from beginning" $ do
            let user = newUser
                expected = R (-8) 0
                actual = R (rank user) (progress user)
            actual `shouldBe` expected
        
        it "should properly support upgrading to next level" $ do
            doTest newUser (-8) (-8) 3
            doTest newUser (-7) (-8) 10
            doTest newUser (-6) (-8) 40
            doTest newUser (-5) (-8) 90
            doTest newUser (-4) (-7) 60
            return ()
        
        it "should properly support upgrading multiple levels" $ do
            doTest newUser (-3) (-6) 50
            doTest newUser (-2) (-5) 60
            doTest newUser (-1) (-4) 90
            doTest newUser 1 (-2) 40
            doTest newUser 2 1 10
            doTest newUser 3 3 0
            doTest newUser 4 5 10
            doTest newUser 5 7 40
            doTest newUser 6 8 0
            doTest newUser 7 8 0
            doTest newUser 8 8 0
            return ()
        
        it "should properly support chain of tasks" $ do
            u <- doTest newUser 1 (-2) 40
            u <- doTest u 1 (-2) 80
            u <- doTest u 1 (-1) 20 
            u <- doTest u 1 (-1) 30 
            u <- doTest u 1 (-1) 40 
            u <- doTest u 2 (-1) 80 
            u <- doTest u 2 1 20 
            u <- doTest u (-1) 1 21 
            u <- doTest u 3 1 61 
            u <- doTest u 8 6 51 
            u <- doTest u 8 6 91 
            u <- doTest u 8 7 31 
            u <- doTest u 8 7 41 
            u <- doTest u 8 7 51 
            u <- doTest u 8 7 61 
            u <- doTest u 8 7 71 
            u <- doTest u 8 7 81 
            u <- doTest u 8 7 91 
            u <- doTest u 8 8 0 
            u <- doTest u 8 8 0 
            return ()
        
        it "should handle invalid range values" $ do
            evaluate (incProgress 9 newUser) `shouldThrow` anyException
            evaluate (incProgress 0 newUser) `shouldThrow` anyException
            evaluate (incProgress (-9) newUser) `shouldThrow` anyException