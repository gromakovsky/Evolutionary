-- | Utility functions to draw charts designed to be used with
-- evolutionary computations algorithms.

module Evolutionary.Chart
       ( drawToFiles
       ) where

import           Control.Lens                           ((.=))
import           Data.List                              (genericIndex,
                                                         genericLength)
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import qualified Graphics.Rendering.Chart.Easy          as CE
import           System.FilePath                        ((<.>), (</>))

drawToFiles
    :: (Num x, Enum x, Fractional x, CE.PlotValue x, CE.PlotValue y)
    => FilePath -> Word -> (x, x) -> (x -> y) -> [[x]] -> IO ()
drawToFiles dir interval range f populations =
    mapM_ (drawToFilesDo dir range f) $
    map
        (\i ->
              (i + 1, populations `genericIndex` i))
        [0, interval .. genericLength populations - 1]

drawToFilesDo
    :: (Num x, Enum x, Fractional x, CE.PlotValue x, CE.PlotValue y)
    => FilePath -> (x, x) -> (x -> y) -> (Word, [x]) -> IO ()
drawToFilesDo dir (lo,hi) f (i, population) = toFile CE.def fileName $ do
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
