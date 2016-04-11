-- | Default values for labs.

module Evolutionary.Defaults
       ( stopCriterion
       , defaultGap
       , populationSizes
       , crossingoverProbabilities
       , mutationProbabilities
       ) where

import           Evolutionary.Genetic (GeneticAlgorithmParams (..),
                                       IterationsCount, PopulationSize)

stopCriterion
    :: Eq a
    => IterationsCount -> [a] -> Bool
stopCriterion cnt prevValues = cnt >= 50 || checkValues
  where
    firstSame = 5
    checkValues =
        length prevValues >= firstSame &&
        all (== head prevValues) (take firstSame prevValues)

defaultGap :: GeneticAlgorithmParams
defaultGap =
    GeneticAlgorithmParams
    { gapPopulationSize = 50
    , gapCrossingoverProbability = 0.6
    , gapMutationProbability = 5.0e-3
    }

populationSizes :: [PopulationSize]
populationSizes = [20, 30, 50, 100, 120, 150, 175, 200, 300]

crossingoverProbabilities :: [Double]
crossingoverProbabilities = [0.5, 0.55, 0.6, 0.7, 0.8, 0.9, 0.95, 1]

mutationProbabilities :: [Double]
mutationProbabilities = [0, 0.002 .. 0.02]
