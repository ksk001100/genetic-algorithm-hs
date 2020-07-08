module Lib
  ( someFunc,
    Gene,
    Individual,
    Population,
    --    initGene,
  )
where

import System.IO.Unsafe
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

--initGene :: Gene
--initGene = do
--  gen <- newStdGen
--  fst $ randomR (Zero, One) gen
