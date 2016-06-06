{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

import           Data.FileEmbed        (embedStringFile, makeRelativeToProject)
import           Data.Foldable         (forM_)
import           Data.List             (genericIndex, genericLength)
import           Data.String           (IsString)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.TimeIt         (timeItT)

import           Serokell.Util         (format', formatSingle')

import           Evolutionary.Chart    (drawRoute, drawStatistics)
import           Evolutionary.Defaults (crossingoverProbabilities,
                                        mutationProbabilities)
import qualified Evolutionary.Defaults as D (defaultGap)
import           Evolutionary.TSP      (CrossoverType (..),
                                        GeneticAlgorithmParams (..),
                                        StopCriterion, geneticTSP, totalWeight)

matrixStr :: IsString s => s
matrixStr = $(makeRelativeToProject "src/lab3/bays29.tsp" >>= embedStringFile)

matrix :: [[Double]]
matrix = map (map (read . T.unpack)) . map T.words . T.lines $ matrixStr

weights :: Word -> Word -> Double
weights a b = matrix `genericIndex` a `genericIndex` b

coordsStr :: IsString s => s
coordsStr = $(makeRelativeToProject "src/lab3/bays29.coords" >>= embedStringFile)

coords :: [(Double, Double)]
coords = map f . map T.words . T.lines $ coordsStr
  where
    f l = (read . T.unpack $ l !! 0, read . T.unpack $ l !! 1)

stopCriterion :: StopCriterion
stopCriterion = (> 120)

bestRoute :: [Word]
bestRoute =
    [ 1
    , 28
    , 6
    , 12
    , 9
    , 26
    , 3
    , 29
    , 5
    , 21
    , 2
    , 20
    , 10
    , 4
    , 15
    , 18
    , 14
    , 17
    , 22
    , 11
    , 19
    , 25
    , 7
    , 23
    , 8
    , 27
    , 16
    , 13
    , 24]

bestWeight :: Double
bestWeight = totalWeight weights $ map pred bestRoute

toDot :: [Word] -> T.Text
toDot route =
    T.unlines $
    ("digraph {" : map showVertex [1 .. length route]) ++
    showEdges route ++ ["}"]
  where
    showVertex = formatSingle' "  {};"
    showEdges (x:y:xs) = format' "  {} -> {};" (x, y) : showEdges (y : xs)
    showEdges _ = []

defaultGap :: GeneticAlgorithmParams
defaultGap =
    D.defaultGap
    { gapPopulationSize = 200
    , gapCrossingoverProbability = 0.8
    , gapMutationProbability = 0.01
    }

findRoute :: GeneticAlgorithmParams -> IO [Word]
findRoute gap = geneticTSP (genericLength matrix) AlternatingEdges weights gap stopCriterion

findTime :: GeneticAlgorithmParams -> IO Double
findTime gap =
    fst <$>
    timeItT
        (geneticTSP
             (genericLength matrix)
             AlternatingEdges
             weights
             gap
             stopCriterion)

findAccuracy :: GeneticAlgorithmParams -> IO Double
findAccuracy gap =
    abs . (bestWeight -) . w <$>
    geneticTSP
        (genericLength matrix)
        AlternatingEdges
        weights
        gap
        stopCriterion
  where
    w = totalWeight weights

main :: IO ()
main = do
    putStrLn "Best route:"
    print bestRoute
    putStrLn "Best length:"
    print bestWeight
    TIO.writeFile "best.dot" $ toDot bestRoute
    () <$ drawRoute "best.png" coords (map pred bestRoute)
    foundRoute <- findRoute defaultGap
    putStrLn "Found route:"
    print . map succ $ foundRoute
    putStrLn "Length is:"
    print $ totalWeight weights foundRoute
    () <$ drawRoute "found.png" coords foundRoute
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
    setXProbability p =
        defaultGap
        { gapCrossingoverProbability = p
        }
    setMutationProbability p =
        defaultGap
        { gapMutationProbability = p
        }
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
    args = [xProbabilityArg, mutationProbabilityArg]
    secondsValue = StatValue "time_s" findTime
    accuracyValue = StatValue "accuracy" findAccuracy
    values = [secondsValue, accuracyValue]

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
    drawStatistics "stats" argName statName xs (action . params)
