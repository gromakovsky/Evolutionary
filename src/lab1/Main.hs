import           Evolutionary.Extremum (argMin)
import           Evolutionary.Genetic  (GeneticAlgorithmParams (..))

f :: Floating a => a -> a
f x = cos (2 * x) / (x * x)

minV :: Double
minV = -20

maxV :: Double
maxV = -2.3

gap :: GeneticAlgorithmParams
gap =
    GeneticAlgorithmParams
    { gapPopulationSize = 100
    , gapCrossingoverProbability = 0.6
    , gapMutationProbability = 1.0e-3
    }

precision :: Double
precision = 0.001

main :: IO ()
main = print =<< argMin precision gap (minV, maxV) f
