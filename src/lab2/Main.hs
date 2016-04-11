{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad                 (when)
import           Data.Foldable                 (forM_)
import           Data.List                     (genericLength, genericReplicate)
import           Data.Tuple.Select             (sel1, sel2, sel3)
import           System.Directory              (createDirectory,
                                                doesDirectoryExist,
                                                removeDirectoryRecursive)
import           System.TimeIt                 (timeItT)

import           Evolutionary.Chart            (drawPopulations3D,
                                                drawStatistics)
import           Evolutionary.Defaults         (crossingoverProbabilities,
                                                defaultGap,
                                                mutationProbabilities,
                                                populationSizes, stopCriterion)
import           Evolutionary.Genetic          (GeneticAlgorithmParams (..),
                                                IterationsCount)
import           Evolutionary.Multidimensional (Point2D (..), Point3D (..),
                                                Range, argMin, metric2D,
                                                metric3D)

mode2D :: Bool
mode2D = False

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

argMinSeconds :: GeneticAlgorithmParams -> IO Double
argMinSeconds
  | mode2D = fmap sel1 . argMinFull2D
  | otherwise = fmap sel1 . argMinFull3D

argMinRes2D :: GeneticAlgorithmParams -> IO (Point2D Double)
argMinRes2D = fmap sel2 . argMinFull2D

argMinRes3D :: GeneticAlgorithmParams -> IO (Point3D Double)
argMinRes3D = fmap sel2 . argMinFull3D

argMinAccuracy :: GeneticAlgorithmParams -> IO Double
argMinAccuracy
  | mode2D = fmap (metric2D actualArgMin2D) . argMinRes2D
  | otherwise = fmap (metric3D actualArgMin3D) . argMinRes3D

argMinPopulations2D :: GeneticAlgorithmParams -> IO [[Point2D Double]]
argMinPopulations2D = fmap sel3 . argMinFull2D

argMinPopulations3D :: GeneticAlgorithmParams -> IO [[Point3D Double]]
argMinPopulations3D = fmap sel3 . argMinFull3D

argMinIterationsCount :: GeneticAlgorithmParams -> IO IterationsCount
argMinIterationsCount
  | mode2D = fmap genericLength . argMinPopulations2D
  | otherwise = fmap genericLength . argMinPopulations3D

average
    :: Real a
    => Word -> IO a -> IO Double
average cnt action = do
    results <- sequence $ genericReplicate cnt action
    return ((fromRational . toRational $ sum results) / fromIntegral cnt)

main :: IO ()
main = do
    putStrLn $ if mode2D then "2D mode" else "3D mode"
    exists <- doesDirectoryExist directoryPath
    when exists $ removeDirectoryRecursive directoryPath
    createDirectory directoryPath
    putStrLn "Running algorithm…"
    putStrLn "Parameters:"
    print defaultGap
    if mode2D then main2D else main3D
    putStrLn "Measuring statistics…"
    measureStatistics

main2D :: IO ()
main2D = do
    (secs,res,populations) <- argMinFull2D defaultGap
    putStrLn $
        mconcat
            [ "Result is "
            , show res
            , ". Generations: "
            , show $ length populations]
    putStrLn $ mconcat ["Algorithm took ", show secs, " seconds"]
    putStrLn "Drawing charts…"
    drawPopulations3D
        directoryPath
        1
        range
        range
        (curry $ f2D . Point2D)
        (map (map getPoint2D) populations)
    putStrLn "Charts are ready"

main3D :: IO ()
main3D = do
    (secs,res,populations) <- argMinFull3D defaultGap
    putStrLn $
        mconcat
            [ "Result is "
            , show res
            , ". Generations: "
            , show $ length populations]
    putStrLn $ mconcat ["Algorithm took ", show secs, " seconds"]

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
    secondsValue = StatValue "time_s" argMinSeconds
    accuracyValue = StatValue "accuracy" argMinAccuracy
    iterationsCountValue = StatValue "iterations count" argMinIterationsCount
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
