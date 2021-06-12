-- https://www.codewars.com/kata/count-the-smiley-faces

module Kata.Kyu6.Smile where
import Data.List ((\\))

countSmileys :: [String] -> Int
countSmileys = length . filter parse
    where
        parse s@[_,_]   = s `elem` smileys
        parse s@[_,_,_] = s `elem` smileys
        parse _         = False
        smileys = noseless ++ nosey
            where
                noseless = map (\\ nose) nosey
                nosey = [[e, n, m] | e <- eyes, n <- nose, m <- mouth]
                eyes = ":;"
                nose = "-~"
                mouth = ")D"