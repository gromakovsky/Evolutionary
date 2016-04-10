{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Simple genetic algorithm.

module Evolutionary.Genetic
       ( IterationsCount
       , PopulationSize
       , FitnessF
       , StopCriterion
       , GeneticAlgorithmParams (..)
       , simpleGA
       ) where

import           Control.Lens            (makeLenses, use, view, (%=), (+=),
                                          (.=))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.RWS       (RWST (runRWST))
import           Control.Monad.Writer    (tell)
import           Data.List               (genericDrop, genericLength, maximumBy,
                                          sortOn)
import           Data.Ord                (comparing)
import           System.Random           (Random)

import           Evolutionary.Individual (Individual (GenerationParams, cross, mutate),
                                          Population, PopulationSize,
                                          randomPopulation)
import           Evolutionary.RandomUtil (frequency, randomBool, randomPairs)

type IterationsCount = Word

type FitnessF i a = i -> a

-- | StopCriterion is a function which takes number of executed
-- iterations, best individuals from previous iterations and returns
-- True iff execution should stop.
type StopCriterion i = IterationsCount -> [i] -> Bool

-- | Parameters for simple genetic algorithm.
data GeneticAlgorithmParams = GeneticAlgorithmParams
    { gapPopulationSize          :: PopulationSize
    , gapCrossingoverProbability :: Double
    , gapMutationProbability     :: Double
    } deriving (Show)

data GAInput i a = GAInput
    { _gaiParams        :: GeneticAlgorithmParams
    , _gaiFitness       :: FitnessF i a
    , _gaiStopCriterion :: StopCriterion i
    }

$(makeLenses ''GAInput)

type GALog i = [Population i]

data GAState i = GAState
    { _gasLastPopulation  :: Population i
    , _gasBestIndividuals :: [i]
    , _gasIterationsCount :: IterationsCount
    }

$(makeLenses ''GAState)

type GAMonad i a = RWST (GAInput i a) (GALog i) (GAState i) IO

-- | Simple genetic algorithm in generic form.
simpleGA
    :: (Individual i, Num a, Ord a, Random a)
    => GeneticAlgorithmParams
    -> GenerationParams i
    -> FitnessF i a
    -> StopCriterion i
    -> IO (i, [Population i])
simpleGA _gaiParams@GeneticAlgorithmParams{..} genParams _gaiFitness _gaiStopCriterion = do
    _gasLastPopulation <-
        randomPopulation gapPopulationSize genParams
    let inp =
            GAInput
            { ..
            }
    let st =
            GAState
            { _gasIterationsCount = 0
            , _gasBestIndividuals = [findBest _gaiFitness _gasLastPopulation]
            , ..
            }
    (individual,_,output) <- runRWST simpleGADo inp st
    return $ (individual, output)

simpleGADo :: (Individual i, Num a, Ord a, Random a) => GAMonad i a i
simpleGADo = do
    res <- simpleGAStep
    criterion <- view gaiStopCriterion
    cnt <- use gasIterationsCount
    bestIndividuals <- use gasBestIndividuals
    if criterion cnt bestIndividuals
        then return res
        else simpleGADo

simpleGAStep
    :: (Individual i, Num a, Ord a, Random a)
    => GAMonad i a i
simpleGAStep = do
    GeneticAlgorithmParams{..} <- view gaiParams
    fitness <- view gaiFitness
    lastPopulation <- use gasLastPopulation
    reproduced <- reproduction fitness lastPopulation
    crossingovered <- crossingover gapCrossingoverProbability reproduced
    mutated <- mutation gapMutationProbability crossingovered
    let newPopulation =
            reduction fitness gapPopulationSize (mutated ++ lastPopulation)
    tell [newPopulation]
    gasLastPopulation .= newPopulation
    gasIterationsCount += 1
    let res = findBest fitness newPopulation
    gasBestIndividuals %= (res:)
    return res

findBest :: Ord a => FitnessF i a -> Population i -> i
findBest fitness population = maximumBy (comparing fitness) population

reproduction
    :: (Num a, Ord a, Random a, MonadIO m)
    => FitnessF i a -> Population i -> m (Population i)
reproduction fitness population =
    liftIO . sequence . replicate (length population) . frequency $
    zip nonNegativeProbs population
  where
    probabilities = map fitness population
    minProb = minimum probabilities
    nonNegativeProbs =
        map (\p -> p - minProb) probabilities

crossingover
    :: (Individual i, MonadIO m)
    => Double -> Population i -> m (Population i)
crossingover prob population =
    liftIO $
    do pairs <- randomPairs $ length population
       individualPairs <- mapM f pairs
       return $ concat individualPairs
  where
    f (idx0, idx1) = do
        let (i0,i1) = (population !! idx0, population !! idx1)
        v <- randomBool prob
        if v
            then cross i0 i1
            else return [i0, i1]

mutation :: (Individual i, MonadIO m) => Double -> Population i -> m (Population i)
mutation p = liftIO . mapM (doMutation p)

doMutation :: Individual i => Double -> i -> IO i
doMutation p i = do
    v <- randomBool p
    if v
        then mutate i
        else return i

reduction :: (Ord a) => FitnessF i a -> PopulationSize -> Population i -> Population i
reduction fitness sz p
  | genericLength p > sz =
      genericDrop (genericLength p - sz) $ sortOn fitness p
  | otherwise = p
