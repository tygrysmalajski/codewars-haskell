-- https://www.codewars.com/kata/valid-parentheses

module Kata.Kyu5.Parentheses where
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

validParentheses :: String -> Bool
validParentheses s = fromMaybe (-1) (process s) == 0
    where
        process = foldM (\i c -> 
            case () of 
            _   | i < 0     -> Nothing
                | c == '('  -> Just (i+1)
                | c == ')'  -> Just (i-1)
                | otherwise -> Just (i)) 0