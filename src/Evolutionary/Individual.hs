{-# LANGUAGE TypeFamilies #-}

-- | Type class defining individuals.

module Evolutionary.Individual
       ( Individual (GenerationParams, randomIndividual, cross, mutate)
       , Population
       , PopulationSize
       , randomPopulation
       ) where

import           Data.List (genericReplicate)

-- | Type class defining individuals.
class Individual a  where
    -- | GenerationParams are used to generate random individual.
    type GenerationParams a :: *
    -- | Generate random individual
    randomIndividual
        :: GenerationParams a -> IO a
    -- | Crossingover generates two individuals from two parents.
    cross
        :: a -> a -> IO [a]
    -- | Mutation slightly changes individual.
    mutate
        :: a -> IO a

-- | Population is a list of individuals.
type Population individual = [individual]
type PopulationSize = Word

randomPopulation
    :: Individual i
    => PopulationSize -> GenerationParams i -> IO (Population i)
randomPopulation sz p = sequence $ genericReplicate sz (randomIndividual p)
