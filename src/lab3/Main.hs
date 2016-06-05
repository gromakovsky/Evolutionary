{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed       (embedStringFile, makeRelativeToProject)
import           Data.String          (IsString)

import           Evolutionary.Genetic (GeneticAlgorithmParams (..),
                                       IterationsCount)

matrixStr :: IsString s => s
matrixStr = $(makeRelativeToProject "src/lab3/bays29.tsp" >>= embedStringFile)

main :: IO ()
main = do
    putStrLn "aaa"
