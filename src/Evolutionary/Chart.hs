-- | Utility functions to draw charts designed to be used with
-- evolutionary computations algorithms.

module Evolutionary.Chart
       ( drawPopulations
       , drawPopulations3D
       , drawStatistics
       , drawPoints3D
       , drawRoute
       ) where

import           Control.Lens                           ((.=))
import           Data.List                              (genericIndex,
                                                         genericLength)
import           Graphics.EasyPlot                      (Color (..),
                                                         Graph2D (..),
                                                         Graph3D (..),
                                                         Option (..), Option3D (RangeX, RangeY),
                                                         Style (Lines),
                                                         TerminalType (PNG),
                                                         plot)
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

drawPopulations3D
    :: FilePath
    -> Word
    -> (Double, Double)
    -> (Double, Double)
    -> (Double -> Double -> Double)
    -> [[(Double, Double)]]
    -> IO ()
drawPopulations3D dir interval xRange yRange f populations =
    mapM_ (drawPopulations3DDo dir xRange yRange f) $
    map
        (\i ->
              (i + 1, populations `genericIndex` i))
        [0,interval .. genericLength populations - 1]

drawPopulations3DDo
    :: FilePath
    -> (Double, Double)
    -> (Double, Double)
    -> (Double -> Double -> Double)
    -> (Word, [(Double, Double)])
    -> IO Bool
drawPopulations3DDo dir (xLo,xHi) (yLo,yHi) f (i,population) =
    plot (PNG fileName) [functionGraph, pointsGraph]
  where
    fileName = dir </> (padLeft '0' 3 $ show i) <.> "png"
    functionGraph = Function3D [Color Green, Title "f"] [RangeX xLo xHi, RangeY yLo yHi] f
    pointsGraph = Data3D [Color Red, Title "Population"] [] pts
    pts = map (\(x, y) -> (x, y, f x y)) population

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

drawPoints3D
    :: FilePath
    -> Word
    -> (Double, Double)
    -> (Double, Double)
    -> [[(Double, Double, Double)]]
    -> IO ()
drawPoints3D dir interval xRange yRange points =
    mapM_ (drawPoints3DDo dir xRange yRange) $
    map
        (\i ->
              (i + 1, points `genericIndex` i))
        [0,interval .. genericLength points - 1]

drawPoints3DDo
    :: FilePath
    -> (Double, Double)
    -> (Double, Double)
    -> (Word, [(Double, Double, Double)])
    -> IO Bool
drawPoints3DDo dir (xLo,xHi) (yLo,yHi) (i,points) =
    plot (PNG fileName) [pointsGraph]
  where
    fileName = dir </> (padLeft '0' 3 $ show i) <.> "png"
    pointsGraph =
        Data3D
            [Color Red, Title "Population"]
            [RangeX xLo xHi, RangeY yLo yHi]
            points

drawRoute :: FilePath -> [(Double, Double)] -> [Word] -> IO Bool
drawRoute fileName coords route = plot (PNG fileName) [points, lns]
  where
    points = Data2D [Color Red, Title "Towns"] [] coords
    lns =
        Data2D [Color Green, Style Lines, Title "Route"] [] $
        map (coords `genericIndex`) route
