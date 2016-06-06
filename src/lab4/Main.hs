{-# LANGUAGE TemplateHaskell #-}

import           Data.List             (genericIndex, genericLength)
import           Data.String           (IsString)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Serokell.Util         (format', formatSingle')

import           Evolutionary.Chart    (drawRoute)
import           Evolutionary.Defaults (defaultGap)
import           Evolutionary.TSP      (CrossoverType (..),
                                        GeneticAlgorithmParams (..),
                                        IterationsCount, StopCriterion,
                                        geneticTSP, totalWeight)

main :: IO ()
main = do
    putStrLn "Here be dragons"
