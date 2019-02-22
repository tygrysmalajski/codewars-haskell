-- https://www.codewars.com/kata/does-my-number-look-big-in-this

module Kata.Kyu6.Narcissistic where
import Data.List
import Data.Maybe

narcissistic :: Integer -> Bool
narcissistic n = (sum . map (^pow) $ digits) == n
    where
        pow = length digits
        digits = unfoldr (\x -> if x > 0 then Just((x `mod` 10), x `div` 10) else Nothing) n