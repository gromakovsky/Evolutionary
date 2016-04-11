{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extremums for mutidimensional functions.

module Evolutionary.Multidimensional
       ( Point2D (..)
       , Point3D (..)
       , Range
       , StopCriterion
       , metric2D
       , metric3D
       , argMax
       , argMin
       ) where

import           System.Random           (Random (randomRIO))

import           Evolutionary.Genetic    (GeneticAlgorithmParams,
                                          IterationsCount, simpleGA)
import           Evolutionary.Individual (Individual (..))

newtype Point2D a = Point2D
    { getPoint2D :: (a, a)
    } deriving (Show, Eq)

newtype Point3D a = Point3D
    { getPoint3D :: (a, a, a)
    } deriving (Show, Eq)

type Range a = (a, a)

type StopCriterion p = IterationsCount -> [p] -> Bool

data RealIndividual a = RealIndividual
    { riValues :: [a]
    , riRanges :: [Range a]
    } deriving (Show)

instance (Random a, Fractional a) => Individual (RealIndividual a) where
    type GenerationParams (RealIndividual a) = [Range a]

    randomIndividual :: [Range a] -> IO (RealIndividual a)
    randomIndividual riRanges = do
        riValues <- mapM randomRIO riRanges
        return $
            RealIndividual
            { ..
            }

    cross :: RealIndividual a -> RealIndividual a -> IO [RealIndividual a]
    cross (RealIndividual v1 rngs) (RealIndividual v2 _) =
        return [RealIndividual v1' rngs, RealIndividual v2' rngs]
      where
        w = 0.6
        v1' = zipWith (+) (map (* w) v1) (map (* (1 - w)) v2)
        v2' = zipWith (+) (map (* w) v2) (map (* (1 - w)) v1)

    mutate :: RealIndividual a -> IO (RealIndividual a)
    mutate i@RealIndividual{..} = do
        idx <- randomRIO (0, length riValues - 1)
        v <- randomRIO $ riRanges !! idx
        return
            i
            { riValues = setListElem idx v riValues
            }
      where
        setListElem idx v l =
            let (beg,end) = splitAt idx l
            in beg ++ (v : tail end)

class RealPoint p where
    riToPoint :: RealIndividual a -> p a

instance RealPoint Point2D where
    riToPoint = riToPoint2D

instance RealPoint Point3D where
    riToPoint = riToPoint3D

metric2D
    :: Floating a
    => Point2D a -> Point2D a -> a
metric2D (Point2D (x,y)) (Point2D (x',y')) =
    sqrt $ (x - x') ** 2 + (y - y') ** 2

metric3D
    :: Floating a
    => Point3D a -> Point3D a -> a
metric3D (Point3D (x,y,z)) (Point3D (x',y',z')) =
    sqrt $ (x - x') ** 2 + (y - y') ** 2 + (z - z') ** 2

riToPoint2D :: RealIndividual a -> Point2D a
riToPoint2D (RealIndividual v _) = Point2D (v !! 0, v !! 1)

riToPoint3D :: RealIndividual a -> Point3D a
riToPoint3D (RealIndividual v _) = Point3D (v !! 0, v !! 1, v !! 2)

argMax
    :: (Fractional a, Random a, RealPoint p, Ord a)
    => GeneticAlgorithmParams
    -> StopCriterion (p a)
    -> [Range a]
    -> (p a -> a)
    -> IO (p a, [[p a]])
argMax gap stopCriterion ranges f =
    convert <$> simpleGA gap ranges (f . riToPoint) stopCriterion'
  where
    convert (ind,populations) =
        (riToPoint ind, map (map riToPoint) populations)
    stopCriterion' c = stopCriterion c . map riToPoint

argMin
    :: (Fractional a, Random a, RealPoint p, Ord a)
    => GeneticAlgorithmParams
    -> StopCriterion (p a)
    -> [Range a]
    -> (p a -> a)
    -> IO (p a, [[p a]])
argMin gap c r f = argMax gap c r (negate . f)
