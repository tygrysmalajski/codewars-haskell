-- https://www.codewars.com/kata/valid-braces

module Kata.Kyu6.Braces (validBraces) where
import Data.List

validBraces :: String -> Bool
validBraces = process []
    where
        process [] [] = True
        process _ [] = False
        process [] (x:xs)
            | open x = process (x:[]) xs
            | otherwise = False
        process (c:opened) (x:xs)
            | open x = process (x:c:opened) xs
            | x `closes` c = process opened xs
            | otherwise = False

open :: Char -> Bool           
open x = elem x "{[("

closes :: Char -> Char -> Bool
closes x y
    | y == '{' && x == '}' = True
    | y == '[' && x == ']' = True
    | y == '(' && x == ')' = True
    | otherwise = False