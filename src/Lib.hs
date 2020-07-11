module Lib
  ( Gene,
    Individual,
    Population,
    initGene,
    initIndividual,
  )
where

import System.IO.Unsafe
import System.Random

data Gene = Zero | One deriving (Show, Eq, Enum, Bounded)

instance Random Gene where
  random g = case randomR (fromEnum Zero, fromEnum One) g of
    (r, g') -> (toEnum r, g')
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')

data Individual = Individual
  { rank :: Int,
    genes :: [Gene]
  }
  deriving (Show)

data Population = Population
  { gene_length :: Int,
    mutate_rate :: Float,
    elite_rate :: Float,
    individuals :: [Individual]
  }
  deriving (Show)

getRandomGene :: IO Gene
getRandomGene = do
  getStdRandom random

initGene :: StdGen -> (Gene, StdGen)
initGene seed = do
  random seed
  
initGenes :: StdGen -> [(Gene, StdGen)]
initGenes seed = do
  let (a, b) = initGene seed
  (a, b) : initGenes b

initIndividual :: Int -> Individual
initIndividual gene_length = do
  let gen = unsafePerformIO newStdGen
      a = take gene_length (initGenes gen)
      b = map fst a
  Individual { rank = length $ filter (== One) b, genes = b }
  
fitness :: Individual -> Individual
fitness individual = individual { rank = length $ filter (== One) (genes individual) }

--mutate :: Individual -> Individual

--crossover :: Individual -> Individual -> Individual
--crossover source target = Individual { rank = 1, genes =  }