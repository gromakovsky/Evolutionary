{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad         (when)
import           Data.Foldable         (forM_)
import           Data.List             (genericLength, genericReplicate)
import           Data.Tuple.Select     (sel1, sel2, sel3)
import           System.Directory      (createDirectory, doesDirectoryExist,
                                        removeDirectoryRecursive)
import           System.TimeIt         (timeItT)

import           Evolutionary.Chart    (drawPopulations, drawStatistics)
import           Evolutionary.Defaults (crossingoverProbabilities, defaultGap,
                                        mutationProbabilities, populationSizes,
                                        stopCriterion)
import           Evolutionary.Extremum (Precision, Range, argMin)
import           Evolutionary.Genetic  (GeneticAlgorithmParams (..),
                                        IterationsCount)

f :: Floating a => a -> a
f x = cos (2 * x) / (x * x)

range :: Range Double
range = (-20, -2.3)

precision :: Precision
precision = 0.00001

actualArgMin :: Double
actualArgMin = -4.6054818

directoryPath :: FilePath
directoryPath = "lab1-charts"

argMinFull :: GeneticAlgorithmParams -> IO (Double, Double, [[Double]])
argMinFull gap = do
    (secs,(res,populations)) <-
        timeItT $ argMin precision gap stopCriterion range f
    return (secs, res, populations)

argMinSeconds :: GeneticAlgorithmParams -> IO Double
argMinSeconds = fmap sel1 . argMinFull

argMinRes :: GeneticAlgorithmParams -> IO Double
argMinRes = fmap sel2 . argMinFull

argMinAccuracy :: GeneticAlgorithmParams -> IO Double
argMinAccuracy = fmap diff . argMinRes
  where
    diff x = abs $ x - actualArgMin

argMinPopulations :: GeneticAlgorithmParams -> IO [[Double]]
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
    putStrLn "Drawing charts…"
    drawPopulations directoryPath 1 range f populations
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
