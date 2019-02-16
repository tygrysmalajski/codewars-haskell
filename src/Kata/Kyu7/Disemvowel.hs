-- https://www.codewars.com/kata/disemvowel-trolls

module Kata.Kyu7.Disemvowel where
import Data.Char

disemvowel :: String -> String
disemvowel = filter $ (`notElem` "aeiou") . toLower