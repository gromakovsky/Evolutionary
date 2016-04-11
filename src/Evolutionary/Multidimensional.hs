{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extremums for mutidimensional functions.

module Evolutionary.Multidimensional
       ( Point2D
       , Point3D
       , Range
       , Rectangle2D
       , StopCriterion
       , argMax2D
       , argMin2D
       ) where

import           System.Random           (Random (randomRIO))

import           Evolutionary.Genetic    (GeneticAlgorithmParams,
                                          IterationsCount, simpleGA)
import           Evolutionary.Individual (Individual (..))

type Point2D a = (a, a)
type Point3D a = (a, a, a)

type Range a = (a, a)

type Rectangle2D a = (Range a, Range a)

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

riToPoint2D :: RealIndividual a -> Point2D a
riToPoint2D (RealIndividual v _) = (v !! 0, v !! 1)

riToPoint3D :: RealIndividual a -> Point3D a
riToPoint3D (RealIndividual v _) = (v !! 0, v !! 1, v !! 2)

argMax2D
    :: GeneticAlgorithmParams
    -> StopCriterion (Point2D Double)
    -> Rectangle2D Double
    -> (Double -> Double -> Double)
    -> IO (Point2D Double, [[Point2D Double]])
argMax2D gap stopCriterion (r0,r1) f =
    convert <$> simpleGA gap [r0, r1] (uncurry f . riToPoint2D) stopCriterion'
  where
    convert (ind,populations) =
        (riToPoint2D ind, map (map riToPoint2D) populations)
    stopCriterion' c = stopCriterion c . map riToPoint2D

argMin2D
    :: GeneticAlgorithmParams
    -> StopCriterion (Point2D Double)
    -> Rectangle2D Double
    -> (Double -> Double -> Double)
    -> IO (Point2D Double, [[Point2D Double]])
argMin2D gap c r f = argMax2D gap c r (\x -> negate . f x)
