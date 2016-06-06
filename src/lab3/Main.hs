{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed        (embedStringFile, makeRelativeToProject)
import           Data.List             (genericIndex, genericLength)
import           Data.String           (IsString)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Serokell.Util         (format', formatSingle')

import           Evolutionary.Defaults (defaultGap)
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

stopCriterion :: StopCriterion
stopCriterion = (> 75)

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

main :: IO ()
main = do
    putStrLn "Best route:"
    print bestRoute
    putStrLn "Best length:"
    print $ totalWeight weights $ map pred bestRoute
    TIO.writeFile "best.dot" $ toDot bestRoute
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
    TIO.writeFile "found.dot" $ toDot $ map succ foundRoute
