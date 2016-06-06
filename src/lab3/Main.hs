{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed        (embedStringFile, makeRelativeToProject)
import           Data.List             (genericIndex, genericLength)
import           Data.String           (IsString)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Serokell.Util         (format', formatSingle')

import           Evolutionary.Chart    (drawRoute)
import qualified Evolutionary.Defaults as D (defaultGap)
import           Evolutionary.TSP      (CrossoverType (..),
                                        GeneticAlgorithmParams (..),
                                        IterationsCount, StopCriterion,
                                        geneticTSP, totalWeight)

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
stopCriterion = (> 350)

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

main :: IO ()
main = do
    putStrLn "Best route:"
    print bestRoute
    putStrLn "Best length:"
    print $ totalWeight weights $ map pred bestRoute
    TIO.writeFile "best.dot" $ toDot bestRoute
    () <$ drawRoute "best.png" coords (map pred bestRoute)
    foundRoute <-
        geneticTSP
            (genericLength matrix)
            AlternatingEdges
            weights
            defaultGap
            stopCriterion
    putStrLn "Found route:"
    print . map succ $ foundRoute
    putStrLn "Length is:"
    print $ totalWeight weights foundRoute
    () <$ drawRoute "found.png" coords foundRoute
