{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.IO             as TIO

import           Serokell.Util            (formatSingle', show')

import qualified Evolutionary.Defaults    as D (defaultGap)
import           Evolutionary.Programming (GeneticAlgorithmParams (..),
                                           StopCriterion, averageValue, delta,
                                           geneticProgramming)

n :: Num a => a
n = 9

f :: [Double] -> Double
f = (10 * n +) . sum . map subexpr
  where
    subexpr x = x ** 2 + 10 * cos (2 * pi * x)

varRange :: (Double, Double)
varRange = (-5.12, 5.12)

defaultGap :: GeneticAlgorithmParams
defaultGap =
    D.defaultGap
    { gapPopulationSize = 40
    , gapCrossingoverProbability = 0.8
    , gapMutationProbability = 0.02
    }

stopCriterion :: StopCriterion
stopCriterion = (> 20)

main :: IO ()
main = do
    res <- geneticProgramming n varRange f defaultGap stopCriterion
    let d = delta n varRange f $ head res
        avg = averageValue n varRange f
    TIO.putStrLn . formatSingle' "Result: {}" $ head res
    TIO.putStrLn . formatSingle' "Delta: {}" $ d
    TIO.putStrLn . formatSingle' "Average: {}" $ avg
    TIO.putStrLn "Results:"
    mapM_ (TIO.putStrLn . show') $ tail res
