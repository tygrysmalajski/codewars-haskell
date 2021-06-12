-- https://www.codewars.com/kata/pick-peaks

module Kata.Kyu5.PickPeak where

data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int] }
    deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks [] = PickedPeaks {pos=[], peaks=[]}
pickPeaks xs = PickedPeaks {pos=pick fst, peaks=pick snd}
    where
        pick f = reverse . f $ result
        xlen = length xs
        result = fst $ foldl process
            (([], []), (1, head xs, xs !! 1, False))
            (drop 2 xs)
            where
                process (state, (i, x1, x, isPlateau)) x2
                    | x1 < x && x > x2
                        = noPlateau . includingX $ state
                    | x1 < x && x == x2
                        = plateau . includingX $ state
                    | isPlateau && (x < x2 || (x == x2 && end))
                        = noPlateau . excludingLast $ state
                    | otherwise
                        = continue asIs state
                    where
                        includingX (positions, peaks) = (i:positions, x:peaks)
                        excludingLast (positions, peaks) = (drop 1 positions, drop 1 peaks)
                        asIs = isPlateau
                        noPlateau = continue False
                        plateau = continue True
                        continue plateau' state' = (state', (i+1, x, x2, plateau'))
                        end = xlen - i-2 == 0