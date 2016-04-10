-- | Utility functions to draw charts designed to be used with
-- evolutionary computations algorithms.

module Evolutionary.Chart
       ( drawPopulations
       , drawStatistics
       ) where

import           Control.Lens                           ((.=))
import           Data.List                              (genericIndex,
                                                         genericLength)
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import qualified Graphics.Rendering.Chart.Easy          as CE
import           System.FilePath                        ((<.>), (</>))

drawPopulations
    :: (Num x, Enum x, Fractional x, CE.PlotValue x, CE.PlotValue y)
    => FilePath -> Word -> (x, x) -> (x -> y) -> [[x]] -> IO ()
drawPopulations dir interval range f populations =
    mapM_ (drawPopulationsDo dir range f) $
    map
        (\i ->
              (i + 1, populations `genericIndex` i))
        [0, interval .. genericLength populations - 1]

drawPopulationsDo
    :: (Num x, Enum x, Fractional x, CE.PlotValue x, CE.PlotValue y)
    => FilePath -> (x, x) -> (x -> y) -> (Word, [x]) -> IO ()
drawPopulationsDo dir (lo,hi) f (i, population) = toFile CE.def fileName $ do
    CE.layout_title .= ("Population #" ++ show i)
    CE.plot $ CE.line "f" [mapToPair [lo, (lo + delta) .. hi]]
    CE.plot $ CE.points "population" $ mapToPair population
  where
    fileName = dir </> (padLeft '0' 3 $ show i) <.> "png"
    delta = (hi - lo) / 1000
    mapToPair = map (\x -> (x, f x))

padLeft :: x -> Word -> [x] -> [x]
padLeft x n xs | genericLength xs >= n = xs
               | otherwise = padLeft x n (x:xs)

drawStatistics
    :: (Real x, Real y)
    => FilePath -> String -> String -> [x] -> (x -> IO y) -> IO ()
drawStatistics dir argName statName args f = do
    stats <- mapM (fmap toDouble . f) args
    toFile CE.def fileName $
        do CE.layout_title .= fullName
           CE.plot $ CE.points statName $ zip (map toDouble args) stats
  where
    toDouble
        :: Real x
        => x -> Double
    toDouble = fromRational . toRational
    fullName = mconcat [statName, " (", argName, ")"]
    fileName = dir </> fullName <.> "png"
