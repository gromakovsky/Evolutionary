{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.IO             as TIO

import           Serokell.Util            (formatSingle', show')

import qualified Evolutionary.Defaults    as D (defaultGap)
import           Evolutionary.Programming (GeneticAlgorithmParams (..),
                                           StopCriterion, geneticProgramming)

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
    { gapPopulationSize = 20
    }

stopCriterion :: StopCriterion
stopCriterion = (> 25)

main :: IO ()
main = do
    res <- geneticProgramming n varRange f defaultGap stopCriterion
    TIO.putStrLn . formatSingle' "Result: {}" $ head res
    TIO.putStrLn "Results:"
    mapM_ (TIO.putStrLn . show') $ tail res
