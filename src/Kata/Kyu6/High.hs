-- https://www.codewars.com/kata/highest-scoring-word

module Kata.Kyu6.High where
import Data.Char
import Data.List
import Data.Ord

high :: String -> String
high [] = ""
high s = topRank . words $ s
    where
        topRank = maximumBy (comparing rank)
        rank = sum . map (subtract 96 . ord)