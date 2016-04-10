{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | BinaryIndividual is an individual represented as sequence of bits
-- with fixed length.

module Evolutionary.BinaryIndividual
       ( BinaryIndividual (..)
       , IndividualLength
       ) where

import           Data.Bits               (Bits (shiftL, setBit, zeroBits, (.|.), (.&.), bit, shiftR, complementBit))
import           Data.List               (genericReplicate)
import           Numeric.Natural         (Natural)
import           System.Random           (Random (randomRIO))

import           Evolutionary.Individual (Individual (randomIndividual, cross, mutate, GenerationParams))
import           Evolutionary.RandomUtil (randomBool)

type IndividualLength = Word

-- | BinaryIndividual is a sequence of bits which has fixed length and
-- is represented as Integer.
data BinaryIndividual = BinaryIndividual
    { biNumber :: Natural
    , biLength :: IndividualLength
    } deriving (Show, Eq)

instance Individual BinaryIndividual where
    type GenerationParams BinaryIndividual = IndividualLength
    randomIndividual :: IndividualLength -> IO BinaryIndividual
    randomIndividual len = do
        bits <- sequence $ genericReplicate len (randomBool 0.5)
        return . flip BinaryIndividual len $ foldr step zeroBits bits
      where
        step False = (`shiftL` 1)
        step True = (`setBit` 0) . (`shiftL` 1)
    cross :: BinaryIndividual -> BinaryIndividual -> IO [BinaryIndividual]
    cross i0 i1 = (\k -> cross' k i0 i1) <$> randomRIO (0, biLength i0 - 1)
    mutate :: BinaryIndividual -> IO BinaryIndividual
    mutate (BinaryIndividual i len) =
        (flip BinaryIndividual len . complementBit i) <$>
        randomRIO (0, (fromIntegral len) - 1)

cross'
    :: Word
    -> BinaryIndividual
    -> BinaryIndividual
    -> [BinaryIndividual]
cross' k (BinaryIndividual i0 len) (BinaryIndividual i1 _) =
    [BinaryIndividual (p00 .|. p11) len, BinaryIndividual (p10 .|. p01) len]
  where
    k' = fromIntegral k
    p00 = leftBits k' i0
    p01 = rightBits k' i0
    p10 = leftBits k' i1
    p11 = rightBits k' i1

leftBits
    :: Bits a
    => Int -> a -> a
leftBits k i = (i `shiftR` k) `shiftL` k

rightBits
    :: Bits a
    => Int -> a -> a
rightBits k i = i .&. (foldr (.|.) zeroBits $ map bit [0 .. k - 1])
