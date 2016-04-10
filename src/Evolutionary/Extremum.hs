-- | Application of genetic algorithm to function maximization problem.

module Evolutionary.Extremum
       ( Range
       , Precision
       , StopCriterion
       , argMax
       , argMin
       ) where

import           Data.Bits                     (shiftL)
import           Numeric.Natural               (Natural)

import           Evolutionary.BinaryIndividual (BinaryIndividual (..),
                                                IndividualLength)
import           Evolutionary.Genetic          (GeneticAlgorithmParams,
                                                IterationsCount, simpleGA)

type Range a = (a, a)

type Precision = Double

type StopCriterion = IterationsCount -> [Double] -> Bool

fromBinary :: Range Double -> BinaryIndividual -> Double
fromBinary (lo, hi) (BinaryIndividual v len) = lo + t * (hi - lo)
  where
    t =
        fromIntegral v /
        (fromIntegral $ (1 `shiftL` (fromIntegral len) :: Natural) - 1)

binaryIndividualLength :: Range Double -> Precision -> IndividualLength
binaryIndividualLength (lo, hi) prec = ceiling $ logBase 2 $ (hi - lo) / prec

argMax
    :: Double
    -> GeneticAlgorithmParams
    -> StopCriterion
    -> Range Double
    -> (Double -> Double)
    -> IO (Double, [[Double]])
argMax precision gap stopCriterion r f = do
    convert <$> simpleGA gap len (f . fromBinary') stopCriterion'
  where
    len = binaryIndividualLength r precision
    fromBinary' = fromBinary r
    convert (ind, populations) =
        (fromBinary' ind, map (map fromBinary') populations)
    stopCriterion' c = stopCriterion c . map fromBinary'

argMin
    :: Double
    -> GeneticAlgorithmParams
    -> StopCriterion
    -> Range Double
    -> (Double -> Double)
    -> IO (Double, [[Double]])
argMin p gap c r f = argMax p gap c r (negate . f)
