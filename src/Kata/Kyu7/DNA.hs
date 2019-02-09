-- https://www.codewars.com/kata/complementary-dna

module Kata.Kyu7.DNA where
import Test.QuickCheck

data Base = A | T | G | C deriving (Eq, Show)

instance Arbitrary Base where
    arbitrary = elements [A, T, G, C]

type DNA = [Base]

dnaStrand :: DNA -> DNA
dnaStrand = map (
    \b -> case b of 
        A -> T 
        T -> A 
        G -> C 
        C -> G)