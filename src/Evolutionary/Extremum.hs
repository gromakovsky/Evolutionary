-- | Application of genetic algorithm to function maximization problem.

module Evolutionary.Extremum
       ( argMax
       , fromIndividual
       , argMin
       ) where

import           Data.Bits            (shiftL)
import           Numeric.Natural      (Natural)

import           Evolutionary.Genetic (GeneticAlgorithmParams, Individual (..),
                                       IndividualLength, genericGA)

type Range a = (a, a)

type Precision = Double

fromIndividual :: Range Double -> IndividualLength -> Individual -> Double
fromIndividual (lo, hi) len (Individual v) = lo + t * (hi - lo)
  where
    t =
        fromIntegral v /
        (fromIntegral $ (1 `shiftL` (fromIntegral len) :: Natural) - 1)

individualLength :: Range Double -> Precision -> IndividualLength
individualLength (lo, hi) prec = ceiling $ logBase 2 $ (hi - lo) / prec

argMax
    :: Double
    -> GeneticAlgorithmParams
    -> Range Double
    -> (Double -> Double)
    -> IO Double
argMax precision gap r f =
    fromIndividual r len <$> genericGA gap len 1000 (f . fromIndividual r len)
  where
    len = individualLength r precision

argMin
    :: Double
    -> GeneticAlgorithmParams
    -> Range Double
    -> (Double -> Double)
    -> IO Double
argMin p gap r f = argMax p gap r (negate . f)
