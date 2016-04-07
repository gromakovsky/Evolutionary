import           System.TimeIt         (timeItT)

import           Evolutionary.Chart    (drawToFiles)
import           Evolutionary.Extremum (argMin)
import           Evolutionary.Genetic  (GeneticAlgorithmParams (..),
                                        IterationsCount)

f :: Floating a => a -> a
f x = cos (2 * x) / (x * x)

minV :: Double
minV = -20

maxV :: Double
maxV = -2.3

gap :: GeneticAlgorithmParams
gap =
    GeneticAlgorithmParams
    { gapPopulationSize = 50
    , gapCrossingoverProbability = 0.6
    , gapMutationProbability = 1.0e-3
    }

precision :: Double
precision = 0.001

stopCriterion :: IterationsCount -> Double -> Double -> Bool
stopCriterion cnt _ _ = cnt >= 24

main :: IO ()
main = do
    putStrLn "Running algorithm…"
    (secs,(res,populations)) <-
        timeItT $ argMin precision gap stopCriterion (minV, maxV) f
    putStrLn $
        mconcat
            [ "Result is "
            , show res
            , ". Generations: "
            , show $ length populations]
    putStrLn $ mconcat ["Algorithm took ", show secs, " seconds"]
    putStrLn "Drawing charts…"
    drawToFiles "tmp" 3 (minV, maxV) f populations
