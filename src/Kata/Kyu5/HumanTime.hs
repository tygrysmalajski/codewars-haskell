-- https://www.codewars.com/kata/human-readable-time

module Kata.Kyu5.HumanTime where
import Data.List
import Text.Printf

humanReadable :: Int -> String
humanReadable = intercalate ":" . map format . units []
    where 
        units xs@[_,_,_] x  = xs
        units xs@[_,_] x    = units (x:xs) 0
        units xs x          = units (x `mod` 60:xs) (x `div` 60)
        format = printf "%02d"