-- https://www.codewars.com/kata/codewars-style-ranking-system

module Kata.Kyu4.CodewarsRanking where
import Data.List((\\))

data User = User { rank::Int, progress::Int }

newUser :: User
newUser = User { rank = minimum ranks, progress = 0 }

incProgress :: Int -> User -> User
incProgress task User { rank = r, progress = p }
    | invalid task || invalid r = error "invalid input"
    | otherwise =
        let (r', p') = if newRank < maximum ranks
            then (newRank, newProgress)
            else (maximum ranks, 0)
        in User { rank = r', progress = p' }
        where
            newRank = let r' = r + dr
                in r' + fromEnum (r < 0 && r' >= 0)
            (dr, newProgress) = (p+dp) `divMod` 100
            dp  | diff == -1 = 1
                | diff == 0  = 3
                | diff >= 1  = 10 * diff^2
                | otherwise  = 0
            diff = if r `div` task >= 0 then d else d-a
                where
                    d = a * abs(r-task)
                    a = toInt $ compare r task
                    toInt x = case x of GT -> -1; EQ -> 0; LT -> 1
            invalid x = x `notElem` ranks

ranks :: [Int]
ranks = [-8..8] \\ [0]