-- | Application of genetic algorithm to function maximization problem.

module Evolutionary.Extremum
       ( Range
       , Precision
       , StopCriterion
       , argMax
       , fromIndividual
       , argMin
       ) where

import           Data.Bits            (shiftL)
import           Numeric.Natural      (Natural)

import           Evolutionary.Genetic (GeneticAlgorithmParams, Individual (..),
                                       IndividualLength, IterationsCount,
                                       simpleGA)

type Range a = (a, a)

type Precision = Double

type StopCriterion = IterationsCount -> [Double] -> Bool

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
    -> StopCriterion
    -> Range Double
    -> (Double -> Double)
    -> IO (Double, [[Double]])
argMax precision gap stopCriterion r f = do
    convert <$> simpleGA gap len (f . fromIndividual') stopCriterion'
  where
    len = individualLength r precision
    fromIndividual' = fromIndividual r len
    convert (ind, populations) =
        (fromIndividual' ind, map (map fromIndividual') populations)
    stopCriterion' c = stopCriterion c . map (fromIndividual r len)

argMin
    :: Double
    -> GeneticAlgorithmParams
    -> StopCriterion
    -> Range Double
    -> (Double -> Double)
    -> IO (Double, [[Double]])
argMin p gap c r f = argMax p gap c r (negate . f)
