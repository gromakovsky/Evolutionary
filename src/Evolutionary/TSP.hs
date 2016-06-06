{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Application of genetic algorithm to travelling salesman problem.

module Evolutionary.TSP
       ( CrossoverType (..)
       , StopCriterion
       , IterationsCount
       , GeneticAlgorithmParams (..)
       , totalWeight
       , geneticTSP
       ) where

import           Control.Lens            (ix, makeLenses, use, view, (&), (.=),
                                          (.~))
import           Control.Monad           (join, replicateM, unless)
import           Control.Monad.Loops     (whileM_)
import           Control.Monad.State     (StateT, execStateT)
import           Control.Monad.Trans     (MonadIO (liftIO))
import           Data.IORef              (newIORef, readIORef, writeIORef)
import           Data.List               (genericIndex, genericLength, nub)
import           Data.Maybe              (catMaybes, fromJust, isJust)
import           System.Random           (Random (randomRIO))
import           System.Random.Shuffle   (shuffleM)

import           Evolutionary.Genetic    (GeneticAlgorithmParams (..),
                                          IterationsCount, simpleGA)
import           Evolutionary.Individual (Individual (..))

data NeighborhoodIndividual = NeighborhoodIndividual
    { niCrossoverType :: CrossoverType
    , niList          :: [Word]
    } deriving (Show)

data CrossoverType
    = AlternatingEdges
    | SubtourChunks
    deriving (Show, Eq)

neighborhoodToSequence :: [Word] -> [Word]
neighborhoodToSequence = (0 :) . neighborhoodToSequence' 0
  where
    neighborhoodToSequence' _ [] = []
    neighborhoodToSequence' i l =
        case l `genericIndex` i of
            0 -> []
            i' -> i' : neighborhoodToSequence' i' l

sequenceToNeighborhood :: [Word] -> [Word]
sequenceToNeighborhood [] = []
sequenceToNeighborhood sq =
    let h = head sq
        n = length sq
        l = fromIntegral $ last sq
    in sequenceToNeighborhood' n sq & ix l .~ h
  where
    sequenceToNeighborhood' n (x:y:xs) =
        sequenceToNeighborhood' n (y : xs) & ix (fromIntegral x) .~ y
    sequenceToNeighborhood' n _ = replicate n 0

data CrossState = CrossState
    { _csNb1 :: [Word]
    , _csNb2 :: [Word]
    , _csRes :: [Maybe Word]
    } deriving (Show)

$(makeLenses ''CrossState)

neighborhoodCross :: Bool -> [Word] -> [Word] -> IO [[Word]]
neighborhoodCross subtour i1 i2 =
    map (map fromJust . view csRes) <$>
    replicateM
        2
        (execStateT (neighborhoodCrossDo True) $
         CrossState i1 i2 $ replicate (length i1) Nothing)

neighborhoodCrossDo :: Bool -> StateT CrossState IO ()
neighborhoodCrossDo useFirst = do
    nb <-
        use $
        if useFirst
            then csNb1
            else csNb2
    let n
            :: Num a
            => a
        n = genericLength nb
    oldRes <- use csRes
    i <- randomRIOWithPredicate (pure . isJust . (oldRes !!)) (0, n - 1)
    csRes . ix i .= Just (nb !! i)
    whileM_ (isBad <$> use csRes) $
        do let p r = elem r . catMaybes <$> use csRes
           r <- randomRIOWithPredicate p (0, n - 1)
           csRes . ix i .= Just r
    newRes <- use csRes
    unless (length (catMaybes newRes) == genericLength nb) $
        neighborhoodCrossDo (not useFirst)

randomRIOWithPredicate :: (MonadIO m, Random a) => (a -> m Bool) -> (a, a) -> m a
randomRIOWithPredicate p r = do
    iRef <- liftIO $ newIORef =<< randomRIO r
    whileM_
        (join $ p <$> liftIO (readIORef iRef))
        (liftIO $ writeIORef iRef =<< randomRIO r)
    liftIO $ readIORef iRef

isBad :: [Maybe Word] -> Bool
isBad l = length l' /= length (nub l') || isCycle l
  where
    l' = catMaybes l

isCycle :: [Maybe Word] -> Bool
isCycle l
  | all isJust l = False
  | otherwise = walk 0 0
  where
    walk acc cur =
        case l `genericIndex` cur of
            Nothing -> False
            Just n ->
                let acc' = acc + 1
                in if acc' > length l
                       then True
                       else walk acc' n

instance Individual NeighborhoodIndividual where
    type GenerationParams NeighborhoodIndividual = (Word, CrossoverType)
    randomIndividual (n,ct) =
        NeighborhoodIndividual ct . sequenceToNeighborhood <$>
        randomShuffle (n - 1)
      where
        randomShuffle = shuffleM . enumFromTo 0
    cross (NeighborhoodIndividual t i1) (NeighborhoodIndividual _ i2) =
        map (NeighborhoodIndividual t) <$>
        neighborhoodCross (t == SubtourChunks) i1 i2
    mutate (NeighborhoodIndividual t nb) =
        NeighborhoodIndividual t . mutateDo <$>
        randomRIO (0, genericLength nb - 1)
      where
        mutateDo i
          | length nb < 3 = nb
          | otherwise =
              let sq = neighborhoodToSequence nb
                  n = length nb
                  j = (i + 1) `mod` n
                  k = (i + 2) `mod` n
                  l = (i + 3) `mod` n
                  sq'
                      :: Num a
                      => Int -> a
                  sq' = fromIntegral . (sq !!)
              in nb & (ix (sq' i) .~ sq' k) . (ix (sq' k) .~ sq' j) .
                 (ix (sq' j) .~ sq' l)

type StopCriterion = IterationsCount -> Bool

type WeightF = Word -> Word -> Double

totalWeight :: WeightF -> [Word] -> Double
totalWeight weights (x:y:xs) = weights x y + totalWeight weights (y : xs)
totalWeight _ _ = 0

geneticTSP :: Word
           -> CrossoverType
           -> WeightF
           -> GeneticAlgorithmParams
           -> StopCriterion
           -> IO [Word]
geneticTSP n ct weights gap sc =
    convertRes <$> simpleGA gap (n, ct) fitness sc'
  where
    fitness = (0 -) . totalWeight weights . neighborhoodToSequence . niList
    sc' i _ = sc i
    convertRes = neighborhoodToSequence . niList . fst
