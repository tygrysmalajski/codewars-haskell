-- https://www.codewars.com/kata/valid-braces

module Kata.Kyu6.Braces (validBraces) where
import Data.List

validBraces :: String -> Bool
validBraces = process []
    where
        process [] [] = True
        process _ [] = False
        process [] (x:xs)
            | open x = process [x] xs
            | otherwise = False
        process opened@(c:cs) (x:xs)
            | open x = process (x:opened) xs
            | x `closes` c = process cs xs
            | otherwise = False

open :: Char -> Bool
open x = x `elem` "{[("

closes :: Char -> Char -> Bool
closes x y
    | y == '{' && x == '}' = True
    | y == '[' && x == ']' = True
    | y == '(' && x == ')' = True
    | otherwise = False