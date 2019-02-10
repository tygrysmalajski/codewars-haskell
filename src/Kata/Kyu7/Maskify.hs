-- https://www.codewars.com/kata/credit-card-mask

module Kata.Kyu7.Maskify where

import Data.List

maskify :: String -> String
maskify str = map (const '#') init' ++ tail'
    where
        (init', tail') = splitAt maskLen str
        maskLen = length str - 4