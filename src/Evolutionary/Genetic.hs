{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Simple genetic algorithm.

module Evolutionary.Genetic
       ( Individual (..)
       , Population
       , IndividualLength
       , IterationsCount
       , PopulationSize
       , FitnessF
       , StopCriterion
       , GeneticAlgorithmParams (..)
       , simpleGA
       ) where

import           Control.Lens            (makeLenses, use, view, (+=), (.=))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.RWS       (RWST (runRWST))
import           Control.Monad.Writer    (tell)
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

-- | StopCriterion is a function which takes number of executed
-- iterations, value on previous iteration, value on last iterations
-- and returns True iff execution should stop.
type StopCriterion a = IterationsCount -> a -> a -> Bool

-- | Parameters for simple genetic algorithm.
data GeneticAlgorithmParams = GeneticAlgorithmParams
    { gapPopulationSize          :: PopulationSize
    , gapCrossingoverProbability :: Double
    , gapMutationProbability     :: Double
    } deriving (Show)

data GAInput a = GAInput
    { _gaiParams           :: GeneticAlgorithmParams
    , _gaiIndividualLength :: IndividualLength
    , _gaiFitness          :: FitnessF a
    , _gaiStopCriterion    :: StopCriterion a
    }

$(makeLenses ''GAInput)

type GALog = [Population]

data GAState = GAState
    { _gasLastPopulation  :: Population
    , _gasIterationsCount :: IterationsCount
    }

$(makeLenses ''GAState)

type GAMonad a = RWST (GAInput a) GALog GAState IO

-- | Simple genetic algorithm in generic form.
simpleGA
    :: (Num a, Ord a, Random a)
    => GeneticAlgorithmParams
    -> IndividualLength
    -> FitnessF a
    -> StopCriterion a
    -> IO (Individual, [Population])
simpleGA _gaiParams@GeneticAlgorithmParams{..} _gaiIndividualLength _gaiFitness _gaiStopCriterion = do
    _gasLastPopulation <- genPopulation gapPopulationSize _gaiIndividualLength
    let inp =
            GAInput
            { ..
            }
    let st =
            GAState
            { _gasIterationsCount = 0
            , ..
            }
    (individual, _, output) <- runRWST simpleGADo inp st
    return $ (individual, output)

simpleGADo :: (Num a, Ord a, Random a) => GAMonad a Individual
simpleGADo = do
    lastPopulation <- use gasLastPopulation
    res <- simpleGAStep
    newPopulation <- use gasLastPopulation
    fitness <- view gaiFitness
    criterion <- view gaiStopCriterion
    cnt <- use gasIterationsCount
    let f = fitness . findBest fitness
    if (criterion cnt (f lastPopulation) (f newPopulation))
        then return res
        else simpleGADo

simpleGAStep
    :: (Num a, Ord a, Random a)
    => GAMonad a Individual
simpleGAStep = do
    GeneticAlgorithmParams{..} <- view gaiParams
    len <- view gaiIndividualLength
    fitness <- view gaiFitness
    lastPopulation <- use gasLastPopulation
    reproduced <- reproduction fitness lastPopulation
    crossingovered <- crossingover gapCrossingoverProbability len reproduced
    mutated <- mutation gapMutationProbability len crossingovered
    newPopulation <- return $ reduction gapPopulationSize mutated
    tell [newPopulation]
    gasLastPopulation .= newPopulation
    gasIterationsCount += 1
    return $ findBest fitness newPopulation

findBest :: Ord a => FitnessF a -> Population -> Individual
findBest fitness population = maximumBy (comparing fitness) population

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
    :: (Num a, Ord a, Random a, MonadIO m)
    => FitnessF a -> Population -> m Population
reproduction fitness population =
    liftIO $
    do let probabilities = map fitness population
       let minProb = minimum probabilities
       let nonNegativeProbs =
               map
                   (\p ->
                         p - minProb)
                   probabilities
       sequence $
           replicate (length population) $
           frequency $ zip nonNegativeProbs population

crossingover :: MonadIO m => Double -> IndividualLength -> Population -> m Population
crossingover prob len population =
    liftIO $
    do pairs <- randomPairs $ length population
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

mutation :: MonadIO m => Double -> IndividualLength -> Population -> m Population
mutation p len = liftIO . mapM (doMutation p len)

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
