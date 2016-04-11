{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad                 (when)
import           Data.Foldable                 (forM_)
import           Data.List                     (genericLength, genericReplicate)
import           Data.Tuple.Select             (sel1, sel2, sel3)
import           System.Directory              (createDirectory,
                                                doesDirectoryExist,
                                                removeDirectoryRecursive)
import           System.TimeIt                 (timeItT)

import           Evolutionary.Chart            (drawPopulations, drawStatistics)
import           Evolutionary.Defaults         (crossingoverProbabilities,
                                                defaultGap,
                                                mutationProbabilities,
                                                populationSizes, stopCriterion)
import           Evolutionary.Genetic          (GeneticAlgorithmParams (..),
                                                IterationsCount)
import           Evolutionary.Multidimensional (Point2D (..), Point3D (..),
                                                Range, argMin, metric2D)

f2D :: Floating a => Point2D a -> a
f2D (Point2D (x, y)) = 100 * (y - x * x) ** 2 + (1 - x) ** 2

f3D :: Floating a => Point3D a -> a
f3D (Point3D (x,y,z)) =
    (100 * (y - x * x) ** 2 + (1 - x) ** 2) +
    (100 * (z - y * y) ** 2 + (1 - y) ** 2)

range :: Range Double
range = (-2.048, 2.048)

r2D :: [Range Double]
r2D = [range, range]

r3D :: [Range Double]
r3D = [range, range, range]

actualArgMin2D :: Point2D Double
actualArgMin2D = Point2D (1, 1)

actualArgMin3D :: Point3D Double
actualArgMin3D = Point3D (1, 1, 1)

directoryPath :: FilePath
directoryPath = "lab2-charts"

argMinFull2D :: GeneticAlgorithmParams
           -> IO (Double, Point2D Double, [[Point2D Double]])
argMinFull2D gap = do
    (secs,(res,populations)) <-
        timeItT $ argMin gap stopCriterion r2D f2D
    return (secs, res, populations)

argMinFull3D :: GeneticAlgorithmParams
           -> IO (Double, Point3D Double, [[Point3D Double]])
argMinFull3D gap = do
    (secs,(res,populations)) <-
        timeItT $ argMin gap stopCriterion r3D f3D
    return (secs, res, populations)

argMinSeconds2D :: GeneticAlgorithmParams -> IO Double
argMinSeconds2D = fmap sel1 . argMinFull2D

argMinRes2D :: GeneticAlgorithmParams -> IO (Point2D Double)
argMinRes2D = fmap sel2 . argMinFull2D

argMinAccuracy2D :: GeneticAlgorithmParams -> IO Double
argMinAccuracy2D = fmap (metric2D actualArgMin2D) . argMinRes2D

argMinPopulations2D :: GeneticAlgorithmParams -> IO [[Point2D Double]]
argMinPopulations2D = fmap sel3 . argMinFull2D

argMinIterationsCount2D :: GeneticAlgorithmParams -> IO IterationsCount
argMinIterationsCount2D = fmap genericLength . argMinPopulations2D

average
    :: Real a
    => Word -> IO a -> IO Double
average cnt action = do
    results <- sequence $ genericReplicate cnt action
    return ((fromRational . toRational $ sum results) / fromIntegral cnt)

main :: IO ()
main = do
    putStrLn "Running algorithm…"
    putStrLn "Parameters:"
    print defaultGap
    (secs, res, populations) <- argMinFull2D defaultGap
    putStrLn $
        mconcat
            [ "Result is "
            , show res
            , ". Generations: "
            , show $ length populations]
    putStrLn $ mconcat ["Algorithm took ", show secs, " seconds"]
    exists <- doesDirectoryExist directoryPath
    when exists $ removeDirectoryRecursive directoryPath
    createDirectory directoryPath
    -- putStrLn "Drawing charts…"
    -- drawPopulations directoryPath 1 range f populations
    putStrLn "Charts are ready"
    putStrLn "Measuring statistics…"
    measureStatistics

data StatArg =
    forall a. Real a => StatArg String
                                [a]
                                (a -> GeneticAlgorithmParams)

data StatValue =
    forall a. Real a => StatValue String
                                  (GeneticAlgorithmParams -> IO a)

measureStatistics :: IO ()
measureStatistics =
    forM_ args $
    \arg ->
         forM_ values $
         \v ->
              measureSomething arg v
  where
    setSize s =
        defaultGap
        { gapPopulationSize = s
        }
    setXProbability p =
        defaultGap
        { gapCrossingoverProbability = p
        }
    setMutationProbability p =
        defaultGap
        { gapMutationProbability = p
        }
    populationSizeArg = StatArg "population size" populationSizes setSize
    xProbabilityArg =
        StatArg
            "crossingover probability"
            crossingoverProbabilities
            setXProbability
    mutationProbabilityArg =
        StatArg
            "mutation probability"
            mutationProbabilities
            setMutationProbability
    args = [populationSizeArg, xProbabilityArg, mutationProbabilityArg]
    secondsValue = StatValue "time_s" argMinSeconds2D
    accuracyValue = StatValue "accuracy" argMinAccuracy2D
    iterationsCountValue = StatValue "iterations count" argMinIterationsCount2D
    values = [secondsValue, accuracyValue, iterationsCountValue]

measureSomething :: StatArg -> StatValue -> IO ()
measureSomething (StatArg argName xs params) (StatValue statName action) =
    measureSomethingDo argName statName xs params action

measureSomethingDo
    :: (Real x, Real y)
    => String
    -> String
    -> [x]
    -> (x -> GeneticAlgorithmParams)
    -> (GeneticAlgorithmParams -> IO y)
    -> IO ()
measureSomethingDo argName statName xs params action =
    drawStatistics directoryPath argName statName xs averaged
  where
    averaged = average 5 . action . params
