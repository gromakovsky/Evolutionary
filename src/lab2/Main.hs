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
import           Evolutionary.Multidimensional (Point2D, Range, Rectangle2D,
                                                argMin2D)

f2D :: Floating a => a -> a -> a
f2D x y = 100 * (y - x * x) ** 2 + (1 - x) ** 2

range :: Range Double
range = (-2.048, 2.048)

r2D :: Rectangle2D Double
r2D = (range, range)

actualArgMin :: Point2D Double
actualArgMin = (1, 1)

directoryPath :: FilePath
directoryPath = "lab2-charts"

argMinFull :: GeneticAlgorithmParams
           -> IO (Double, Point2D Double, [[Point2D Double]])
argMinFull gap = do
    (secs,(res,populations)) <-
        timeItT $ argMin2D gap stopCriterion r2D f2D
    return (secs, res, populations)

argMinSeconds :: GeneticAlgorithmParams -> IO Double
argMinSeconds = fmap sel1 . argMinFull

argMinRes :: GeneticAlgorithmParams -> IO (Point2D Double)
argMinRes = fmap sel2 . argMinFull

argMinAccuracy :: GeneticAlgorithmParams -> IO Double
argMinAccuracy = fmap diff . argMinRes
  where
    diff (x, y) =
        sqrt $ (x - fst actualArgMin) ** 2 + (y - snd actualArgMin) ** 2

argMinPopulations :: GeneticAlgorithmParams -> IO [[Point2D Double]]
argMinPopulations = fmap sel3 . argMinFull

argMinIterationsCount :: GeneticAlgorithmParams -> IO IterationsCount
argMinIterationsCount = fmap genericLength . argMinPopulations

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
    (secs, res, populations) <- argMinFull defaultGap
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
