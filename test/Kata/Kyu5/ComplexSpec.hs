module Kata.Kyu5.ComplexSpec where

import Kata.Kyu5.Complex
import Data.Ratio
import Test.Hspec

import Test.QuickCheck

Complex a b =~ Complex c d = abs (a-c) + abs (b-d) < 1e-6
infix 4 =~

spec :: Spec
spec = do
  describe "Num instance for complex floats" $ do
    let complex a b  = Complex a b :: Complex Float
    it "can add complex numbers" $ do
      property $ \a b c d -> complex a b + complex c d == complex (a+c) (b+d)
    it "can subtract complex numbers" $ do
      property $ \a b c d -> complex a b - complex c d == complex (a-c) (b-d)
    it "can multiply complex numbers" $ do
      property $ \a b c d -> complex a b * complex c d == complex (a*c - b*d) (a*d + b*c)
    it "can negate complex numbers" $ do
      property $ \a b -> negate (complex a b) == complex (negate a) (negate b)
    it "can compute complex absolute values" $ do
      property $ \a b -> abs (complex a b) == complex (sqrt $ a*a + b*b) 0
    it "computes the correct signum for 0" $ do
      signum (complex 0 0) `shouldBe` complex 0 0
    it "can compute general complex signum" $ do
      property $ \(NonZero a) (NonZero b) ->
        let l = sqrt (a*a + b*b)
        in signum (complex a b) == complex (a/l) (b/l)
  describe "Fractional instance for complex floats" $ do
    let complex a b  = Complex a b :: Complex Float
    it "can divide complex numbers" $ do
      property $ \(NonZero a) (NonZero b) (NonZero c) (NonZero d) ->
        complex a b / complex c d =~ complex ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))
    it "can invert complex numbers" $ do
      property $ \(NonZero a) (NonZero b) ->
        recip (complex a b) == complex (a / (a*a + b*b)) (-b / (a*a + b*b))
  describe "Num instance for complex doubles" $ do
    let complex a b  = Complex a b :: Complex Double
    it "can add complex numbers" $ do
      property $ \a b c d -> complex a b + complex c d == complex (a+c) (b+d)
    it "can subtract complex numbers" $ do
      property $ \a b c d -> complex a b - complex c d == complex (a-c) (b-d)
    it "can multiply complex numbers" $ do
      property $ \a b c d -> complex a b * complex c d == complex (a*c - b*d) (a*d + b*c)
    it "can negate complex numbers" $ do
      property $ \a b -> negate (complex a b) == complex (negate a) (negate b)
    it "can compute complex absolute values" $ do
      property $ \a b -> abs (complex a b) == complex (sqrt $ a*a + b*b) 0
    it "computes the correct signum for 0" $ do
      signum (complex 0 0) `shouldBe` complex 0 0
    it "can compute general complex signum" $ do
      property $ \(NonZero a) (NonZero b) ->
        let l = sqrt (a*a + b*b)
        in signum (complex a b) == complex (a/l) (b/l)
  describe "Fractional instance for complex doubles" $ do
    let complex a b  = Complex a b :: Complex Float
    it "can divide complex numbers" $ do
      property $ \(NonZero a) (NonZero b) (NonZero c) (NonZero d) ->
        complex a b / complex c d =~ complex ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))
    it "can invert complex numbers" $ do
      property $ \(NonZero a) (NonZero b) ->
        recip (complex a b) == complex (a / (a*a + b*b)) (-b / (a*a + b*b))
  describe "Type conversions" $ do
    it "creates Complex Floats from Integers" $ do
      property $ \n -> (fromInteger n :: Complex Float) == Complex (fromInteger n) 0
    it "creates Complex Doubles from Integers" $ do
      property $ \n -> (fromInteger n :: Complex Double) == Complex (fromInteger n) 0
    it "creates Comples Floats from Rationals" $ do
      property $ \a (NonZero b) -> (fromRational (a % b) :: Complex Float) == Complex (fromRational $ a % b) 0
    it "creates Comples Floats from Rationals" $ do
      property $ \a (NonZero b) -> (fromRational (a % b) :: Complex Double) == Complex (fromRational $ a % b) 0
  describe "Complex number representations" $ do
    it "extracts the real parts" $ do
      property $ \a b -> real (Complex a b :: Complex Double) == a
    it "extracts the real imaginary" $ do
      property $ \a b -> imaginary (Complex a b :: Complex Double) == b
    it "converts to cartesian representation" $ do
      property $ \a b -> cartesian (Complex a b :: Complex Double) == (a,b)
    it "extracts the magnitude" $ do
      property $ \a b -> magnitude (Complex a b :: Complex Double) == sqrt (a*a + b*b)
    it "extracts the phase" $ do
      property $ \a b -> phase (Complex a b :: Complex Double) == atan2 b a
    it "converts to polar representation" $ do
      property $ \a b -> polar (Complex a b :: Complex Double) == (sqrt $ a*a + b*b, atan2 b a)
  describe "Complex conjugation" $ do
    it "can be computed" $ do
      property $ \a b -> conjugate (Complex a b :: Complex Float) == Complex a (-b)
    it "is compatible with absolute value" $ do
      property $ \a b -> Complex a b * conjugate (Complex a b :: Complex Float) == Complex (a*a + b*b) 0