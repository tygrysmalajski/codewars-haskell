-- https://www.codewars.com/kata/59e74fb5fc3c49f1e2000028

module Kata.Kyu5.Complex where

data Complex a = Complex a a
    deriving (Eq, Show)

instance RealFloat a => Num (Complex a) where
    (+) (Complex x y) (Complex u v) = Complex (x+u) (y+v)
    (-) (Complex x y) (Complex u v) = Complex (x-u) (y-v)
    (*) (Complex x y) (Complex u v) = Complex (x*u - y*v) (x*v + y*u)
    abs (Complex x y) = Complex (sqrt $ x^2 + y^2) 0
    signum 0 = 0
    signum c@(Complex x y) = Complex (x/r) (y/r) where r = magnitude c
    fromInteger x = Complex (fromInteger x) 0

instance RealFloat a => Fractional (Complex a) where
    (/) _ c2@(Complex 0 0) = c2
    (/) (Complex x y) (Complex u v) = Complex real imaginary
        where
            real = (x*u + y*v) / (u^2 + v^2)
            imaginary = (y*u - x*v) / (u^2 + v^2)
    recip c@(Complex 0 0) = c
    recip (Complex x y) = Complex (x / (x^2 + y^2)) (-y / (x^2 + y^2))
    fromRational x = Complex (fromRational x) 0

real, imaginary, magnitude, phase :: RealFloat a => Complex a -> a
real (Complex x _) = x
imaginary (Complex _ y) = y
magnitude = real . abs
phase (Complex x y) = atan2 y x

cartesian, polar :: RealFloat a => Complex a -> (a, a)
cartesian c = (real c, imaginary c)
polar c = (magnitude c, phase c)

conjugate :: RealFloat a => Complex a -> Complex a
conjugate (Complex x y) = Complex x (-y)