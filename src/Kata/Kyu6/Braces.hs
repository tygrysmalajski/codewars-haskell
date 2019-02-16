-- https://www.codewars.com/kata/valid-braces

module Kata.Kyu6.Braces where

validBraces :: String -> Bool
validBraces xs = foldl process (0, 0, 0) xs == (0, 0, 0)
    where
        process (c, b, p) x
            | c < 0 || b < 0 || p < 0 = (c, b, p)
            | x == '}' = curly dec
            | x == '{' = curly inc
            | x == '[' = bracket inc
            | x == ']' = bracket dec
            | x == '(' = paranthese inc
            | x == ')' = paranthese dec
            where
                curly f = (f c, b, p)
                bracket f = (c, f b, p)
                paranthese f = (c, b, f p)
                inc x' = x' + 1
                dec x' = x' - 1