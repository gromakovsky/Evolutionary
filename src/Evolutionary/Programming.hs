{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Application of genetic algorithm to genetic programming.

module Evolutionary.Programming
       ( Node (..)
       , Terminal (..)
       , UnaryOp (..)
       , BinaryOp (..)
       , StopCriterion
       , IterationsCount
       , GeneticAlgorithmParams (..)
       , fitness
       , delta
       , averageValue
       , geneticProgramming
       ) where

import           Control.Monad           (replicateM)
import           Data.List               (genericIndex, genericLength)
import           Data.Text.Buildable     (Buildable (build))
import qualified Data.Text.Format        as F
import           System.Random           (Random (randomRIO))

import           Evolutionary.Genetic    (FitnessF, GeneticAlgorithmParams (..),
                                          IterationsCount, findBest, simpleGA)
import           Evolutionary.Individual (Individual (..))

data Node
    = TNode !Terminal
    | UNode !UnaryOp
            !Node
    | BNode !BinaryOp
            !Node
            !Node
    deriving (Show)

data Terminal
    = TermVar !Word
    | TermPi
    | TermTen
    | TermTwo
    | TermZero
    deriving (Show)

data UnaryOp
    = UAbs
    | USin
    | UCos
    | UExp
    deriving (Show)

data BinaryOp
    = BPlus
    | BMinus
    | BMult
    | BDiv
    | BPower
    deriving (Show)

instance Buildable Node where
    build (TNode t) = build t
    build (UNode UAbs t) = F.build "abs({})" $ F.Only t
    build (UNode USin t) = F.build "sin({})" $ F.Only t
    build (UNode UCos t) = F.build "cos({})" $ F.Only t
    build (UNode UExp t) = F.build "exp({})" $ F.Only t
    build (BNode BPlus t1 t2) = F.build "({} + {})" (t1, t2)
    build (BNode BMinus t1 t2) = F.build "({} - {})" (t1, t2)
    build (BNode BMult t1 t2) = F.build "({} * {})" (t1, t2)
    build (BNode BDiv t1 t2) = F.build "({} / {})" (t1, t2)
    build (BNode BPower t1 t2) = F.build "{} ** {}" (t1, t2)

instance Buildable Terminal where
    build (TermVar i) = F.build "x_{}" $ F.Only i
    build TermPi = "pi"
    build TermTen = "10"
    build TermTwo = "2"
    build TermZero = "0"

randomNode :: Word -> Word -> IO Node
randomNode maxDepth varsNum = do
    t <- randomRIO (0, if maxDepth == 0 then 0 else 4)
    let m' = maxDepth - 1
    case t of
        (0 :: Int) -> TNode <$> randomTerminal varsNum
        1 -> UNode <$> randomUOp <*> randomNode m' varsNum
        _ -> BNode <$> randomBOp <*> randomNode m' varsNum <*>
             randomNode m' varsNum

randomTerminal :: Word -> IO Terminal
randomTerminal varsNum = do
    t <- randomRIO (0, 6)
    case t of
        (0 :: Int) -> TermVar <$> randomRIO (0, varsNum - 1)
        (1 :: Int) -> TermVar <$> randomRIO (0, varsNum - 1)
        (2 :: Int) -> TermVar <$> randomRIO (0, varsNum - 1)
        3 -> pure TermPi
        4 -> pure TermTen
        5 -> pure TermTwo
        _ -> pure TermZero

randomUOp :: IO UnaryOp
randomUOp = ([UAbs, USin, UCos, UExp] !!) <$> randomRIO (0, 3)

randomBOp :: IO BinaryOp
randomBOp = ([BPlus, BMinus, BMult, BDiv, BPower] !!) <$> randomRIO (0, 4)

class Eval a  where
    eval :: a -> [Double] -> Double

instance Eval Node where
    eval (TNode t) = eval t
    eval (UNode UAbs n) = abs . eval n
    eval (UNode USin n) = sin . eval n
    eval (UNode UCos n) = cos . eval n
    eval (UNode UExp n) = exp . eval n
    eval (BNode BPlus m n) = \a -> eval m a + eval n a
    eval (BNode BMinus m n) = \a -> eval m a - eval n a
    eval (BNode BMult m n) = \a -> eval m a * eval n a
    eval (BNode BDiv m n) = \a -> eval m a `safeDiv` eval n a
    eval (BNode BPower m n) = \a -> eval m a `safePower` eval n a

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 0
safeDiv a b = a / b

safePower :: Double -> Double -> Double
safePower 0 0 = 0
safePower a b = abs a ** b

isGood :: Double -> Bool
isGood n = not (isNaN n) && not (isInfinite n)

myEval :: Node -> [Double] -> Double
myEval n args
  | isNaN res = 0
  | isInfinite res = 1.0e10
  | otherwise = res
  where
    res = eval n args

instance Eval Terminal where
    eval (TermVar i) vars = vars `genericIndex` i
    eval TermPi _ = pi
    eval TermTen _ = 10
    eval TermTwo _ = 2
    eval TermZero _ = 0

data ProgrammingIndividual = ProgrammingIndividual
    { piNode    :: Node
    , piVarsNum :: Word
    } deriving (Show)

instance Individual ProgrammingIndividual where
    type GenerationParams ProgrammingIndividual = (Word, Word)
    randomIndividual (maxDepth,varsNum) =
        ProgrammingIndividual <$> randomNode maxDepth varsNum <*> pure varsNum
    cross (ProgrammingIndividual n1 varsNum) (ProgrammingIndividual n2 _) = do
        (n1',n2') <- crossNodes n1 n2
        return . map (flip ProgrammingIndividual varsNum) $ [n1', n2']
    mutate (ProgrammingIndividual node varsNum) =
        flip ProgrammingIndividual varsNum <$>
        (mutateDo varsNum node =<< randomPath node)

type Path = [Word]

mutateDo :: Word -> Node -> Path -> IO Node
mutateDo varsNum (TNode _) [] = TNode <$> randomTerminal varsNum
mutateDo _ (UNode _ n1) [] = UNode <$> randomUOp <*> pure n1
mutateDo _ (BNode _ n1 n2) [] = BNode <$> randomBOp <*> pure n1 <*> pure n2
mutateDo varsNum (UNode op n1) (0:xs) = UNode op <$> mutateDo varsNum n1 xs
mutateDo varsNum (BNode op n1 n2) (0:xs) = BNode op <$> mutateDo varsNum n1 xs <*> pure n2
mutateDo varsNum (BNode op n1 n2) (1:xs) = BNode op <$> pure n1 <*> mutateDo varsNum n2 xs
mutateDo _ n p = error $ mconcat ["mutateDo: ", show n, ", ", show p]

randomPath :: Node -> IO Path
randomPath (TNode _) = pure []
randomPath (UNode _ n) = do
    t <- randomRIO (0, 1)
    case t of
        (0 :: Int) -> pure []
        _ -> (0 :) <$> randomPath n
randomPath (BNode _ n1 n2) = do
    t <- randomRIO (0, 2)
    case t of
        (0 :: Int) -> pure []
        1 -> (0 :) <$> randomPath n1
        _ -> (1 :) <$> randomPath n2

-- | Return node and depth.
walk :: Node -> Path -> (Node, Word)
walk n@(TNode _) _ = (n, 0)
walk n [] = (n, 0)
walk (UNode _ n) (0:xs) =
    let (t',d') = walk n xs
    in (t', d' + 1)
walk (BNode _ n _) (0:xs) =
    let (t',d') = walk n xs
    in (t', d' + 1)
walk (BNode _ _ n) (1:xs) =
    let (t',d') = walk n xs
    in (t', d' + 1)
walk n p = error $ mconcat ["walk: ", show n, ", ", show p]

changeSubTree :: Node -> Path -> Node -> Node
changeSubTree subTree [] _ = subTree
changeSubTree subTree (0:xs) (UNode op n) =
    UNode op $ changeSubTree subTree xs n
changeSubTree subTree (0:xs) (BNode op n1 n2) =
    BNode op (changeSubTree subTree xs n1) n2
changeSubTree subTree (1:xs) (BNode op n1 n2) =
    BNode op n1 (changeSubTree subTree xs n2)
changeSubTree s p t = error $ mconcat ["changeSubTree: ", show s, ", ", show p, ", ", show t]

crossNodes :: Node -> Node -> IO (Node, Node)
crossNodes n1 n2 = do
    p1 <- randomPath n1
    p2 <- randomPath n2
    let (n1',_) = walk n1 p1
        (n2',_) = walk n2 p2
    return (changeSubTree n2' p1 n1, changeSubTree n1' p2 n2)

type StopCriterion = IterationsCount -> Bool

genArgs :: Word -> (Double, Double) -> [[Double]]
genArgs varsNum (varMin, varMax) =
    replicateM
        (fromIntegral varsNum)
        [varMin,varMin + (varMax - varMin) / 2 .. varMax]

fitness :: (Double, Double) -> ([Double] -> Double) -> FitnessF ProgrammingIndividual Double
fitness r f ProgrammingIndividual{..}
  | isGood res = res
  | otherwise = -1.0e10
  where
    res = (0 -) . sum . map (** 2) . map (diff piNode) $ genArgs piVarsNum r
    diff n a = f a - myEval n a

delta :: Word -> (Double, Double) -> ([Double] -> Double) -> Node -> Double
delta varsNum r f node =
    sqrt . abs $
    (fitness r f $ ProgrammingIndividual node varsNum) /
    genericLength (genArgs varsNum r)

averageValue :: Word -> (Double, Double) -> ([Double] -> Double) -> Double
averageValue varsNum r f =
    sqrt $ sum (map ((** 2) . f) args) / genericLength args
  where
    args = genArgs varsNum r

geneticProgramming :: Word
                   -> (Double, Double)
                   -> ([Double] -> Double)
                   -> GeneticAlgorithmParams
                   -> StopCriterion
                   -> IO [Node]
geneticProgramming varsNum range f gap sc =
    convertRes <$> simpleGA gap (5, varsNum) (fitness range f) sc'
  where
    sc' i _ = sc i
    convertRes (res,populations) =
        map piNode $ res : map convertResDo populations
    convertResDo = findBest (fitness range f)
