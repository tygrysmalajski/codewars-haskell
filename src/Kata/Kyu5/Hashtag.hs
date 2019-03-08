-- https://www.codewars.com/kata/the-hashtag-generator

module Kata.Kyu5.Hashtag where
import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag s
    | length s > 140 = Nothing
    | all nonLetter s = Nothing
    | otherwise = Just . reverse . fst . foldl build ("#", toUpper) $ s
    where
        build (hashtag, f) c
            | nonLetter c = (hashtag, toUpper)
            | otherwise = (f c:hashtag, id)
        nonLetter = not . isLetter