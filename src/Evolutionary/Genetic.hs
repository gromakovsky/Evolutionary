{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple genetic algorithm.

module Evolutionary.Genetic
       ( Individual (..)
       , Population
       , IndividualLength
       , IterationsCount
       , PopulationSize
       , FitnessF
       , GeneticAlgorithmParams (..)
       , genericGA
       ) where

import           Data.Bits               (Bits (shiftL, setBit, zeroBits, (.|.), (.&.), bit, shiftR, complementBit))
import           Data.List               (genericLength, genericReplicate,
                                          maximumBy)
import           Data.Ord                (comparing)
import           Numeric.Natural         (Natural)
import           System.Random           (Random (randomRIO))

import           Evolutionary.RandomUtil (frequency, randomBool, randomPairs)

-- | Individual is a sequence of bits represented as Integer.
newtype Individual = Individual
    { getIndividual :: Natural
    } deriving (Show, Eq, Bits)

-- | Population is a list of individuals.
type Population = [Individual]

-- | Some trivial type aliases to make other types more clear.
type IndividualLength = Word
type IterationsCount = Word
type PopulationSize = Word

type FitnessF a = Individual -> a

-- | Parameters for simple genetic algorithm.
data GeneticAlgorithmParams = GeneticAlgorithmParams
    { gapPopulationSize          :: PopulationSize
    , gapCrossingoverProbability :: Double
    , gapMutationProbability     :: Double
    } deriving (Show)

-- | Generic simple genetic algorithm.
genericGA
    :: (Num a, Ord a, Random a)
    => GeneticAlgorithmParams
    -> IndividualLength
    -> IterationsCount
    -> FitnessF a
    -> IO Individual
genericGA p@GeneticAlgorithmParams{..} len iterCnt fitness =
    genericGAStep p len iterCnt fitness =<< genPopulation gapPopulationSize len

genericGAStep
    :: (Num a, Ord a, Random a)
    => GeneticAlgorithmParams
    -> IndividualLength
    -> IterationsCount
    -> FitnessF a
    -> Population
    -> IO Individual
genericGAStep _ _ 0 fitness population =
    return $ maximumBy (comparing fitness) population
genericGAStep p@GeneticAlgorithmParams{..} len iterations fitness population = do
    reproduced <- reproduction fitness population
    crossingovered <- crossingover gapCrossingoverProbability len reproduced
    mutated <- mutation gapMutationProbability len crossingovered
    let reduced = reduction gapPopulationSize mutated
    genericGAStep p len (iterations - 1) fitness reduced

genIndividual :: Word -> IO Individual
genIndividual len = do
    bits <- sequence $ genericReplicate len (randomBool 0.5)
    return $ foldr step zeroBits bits
  where
    step False = (`shiftL` 1)
    step True = (`setBit` 0) . (`shiftL` 1)

genPopulation :: PopulationSize -> IndividualLength -> IO Population
genPopulation sz len = sequence $ genericReplicate sz (genIndividual len)

reproduction
    :: (Num a, Ord a, Random a)
    => FitnessF a -> Population -> IO Population
reproduction fitness population = do
    let probabilities = map fitness population
    let minProb = minimum probabilities
    let nonNegativeProbs = map (\p -> p - minProb) probabilities
    sequence $
        replicate (length population) $
        frequency $ zip nonNegativeProbs population

crossingover :: Double -> IndividualLength -> Population -> IO Population
crossingover prob len population = do
    pairs <- randomPairs $ length population
    individualPairs <- mapM f pairs
    return $ flattenPairs individualPairs
  where
    f (idx0,idx1) = do
        let (i0,i1) = (population !! idx0, population !! idx1)
        v <- randomBool prob
        k <- randomRIO (0, len - 1)
        if v
            then return $ cross k i0 i1
            else return $ (i0, i1)
    flattenPairs [] = []
    flattenPairs ((x,y):xs) = x : y : flattenPairs xs

cross :: Word -> Individual -> Individual -> (Individual, Individual)
cross k i0 i1 = (p00 .|. p11, p10 .|. p01)
  where
    k' = fromIntegral k
    p00 = leftBits k' i0
    p01 = rightBits k' i0
    p10 = leftBits k' i1
    p11 = rightBits k' i1

leftBits :: Bits a => Int -> a -> a
leftBits k i = (i `shiftR` k) `shiftL` k

rightBits :: Bits a => Int -> a -> a
rightBits k i = i .&. (foldr (.|.) zeroBits $ map bit [0 .. k - 1])

mutation :: Double -> IndividualLength -> Population -> IO Population
mutation p len = mapM (doMutation p len)

doMutation :: Double -> IndividualLength -> Individual -> IO Individual
doMutation p len i = do
    v <- randomBool p
    if v
        then changeRandomBit
        else return i
  where
    changeRandomBit = complementBit i <$> randomRIO (0, (fromIntegral len) - 1)

reduction :: PopulationSize -> Population -> Population
reduction sz p
  | genericLength p > sz = reduction sz $ tail p
  | otherwise = p
