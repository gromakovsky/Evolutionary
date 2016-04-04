-- | Random utilities.

module Evolutionary.RandomUtil
       ( randomBool
       , frequency
       , randomPairs
       ) where

import           System.Random (Random (randomRIO))

-- | Generate True with given probability.
randomBool :: Double -> IO Bool
randomBool prob =
    (\v ->
          v < prob) <$>
    randomRIO (0, 1)

-- | Choose random value from list, taking probabilities into account.
-- Probabilities are assumed to be non-negative.
frequency
    :: (Num prob, Ord prob, Random prob)
    => [(prob, v)] -> IO v
frequency xs0 = do
    r <- randomRIO (0, sum $ map fst xs0)
    return $ pick r xs0
  where
    pick r ((p,x):xs)
      | r <= p = x
      | otherwise = pick (r - p) xs
    pick _ _ = error "Empty list passed to frequency"

-- | Generate list of pairs of numbers in range [0 .. v - 1].
-- If length of list is odd, there will be one pair with two same
-- values.
randomPairs :: Int -> IO [(Int, Int)]
randomPairs v = randomPairsDo [0 .. v - 1] []

-- TODO
randomPairsDo :: [Int] -> [(Int, Int)] -> IO [(Int, Int)]
randomPairsDo [] res = return res
randomPairsDo [x] res = return $ (x, x) : res
randomPairsDo (x:y:xs) res =
    (\v ->
          (x, y) : (v ++ res)) <$>
    randomPairsDo xs res
