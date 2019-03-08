module Kata.Kyu5.Baker where

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum . map toSupplies $ recipe
    where
        toSupplies (ingredient, required) = maybe 0 (`div` required) . lookup ingredient $ storage