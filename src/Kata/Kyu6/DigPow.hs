-- https://www.codewars.com/kata/playing-with-digits

module Kata.Kyu6.DigPow where

import Data.List

digpow :: Integer -> Integer -> Integer
digpow n p
    | digsum `mod` n == 0 = digsum `div` n
    | otherwise = -1
    where 
        digsum = fst . foldr (\d (s, p') -> (s + d^p', p'+1)) (0, p) $ digits
        digits = unfoldr (\n' -> 
            if n' > 0 then Just(n' `mod` 10, n' `div` 10) 
            else Nothing) n