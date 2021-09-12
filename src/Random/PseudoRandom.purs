module Random.PseudoRandom
    ( RandomPair
    , class Random
    , class RandomR
    , random
    , randomR
    , randoms
    , randomRs
    , randomEff
    , randomREff
    , module ReExportLCG
    ) where

import Prelude

import Control.Monad.ST (ST, run, for)
import Control.Monad.ST.Ref (new, read, write)
import Data.Array.ST (STArray, push, withArray)
import Data.Enum (fromEnum, toEnumWithDefaults)
import Data.Int (toNumber)
import Effect (Effect)
import Random.LCG (Seed, lcgM, lcgNext, randomSeed, unSeed)
import Random.LCG (Seed, mkSeed, unSeed, randomSeed) as ReExportLCG

type RandomPair a =
  { newVal :: a
  , newSeed :: Seed
  }

class Random a where
  random :: Seed -> RandomPair a

instance randomInt :: Random Int where
  random seed = { newVal: unSeed newSeed, newSeed: newSeed }
    where newSeed = lcgNext seed

instance randomNumber :: Random Number where
  random seed = { newVal: newVal, newSeed: intRp.newSeed }
    where
      intRp = random seed
      newVal = toNumber intRp.newVal / toNumber lcgM

instance randomBoolean :: Random Boolean where
  random seed = { newVal: newVal, newSeed: numRp.newSeed }
    where
      numRp = random seed
      newVal = numRp.newVal > 0.5

instance randomChar :: Random Char where
  random seed = { newVal: newVal, newSeed: intRp.newSeed }
    where
      intRp = randomR 0 65535 seed
      newVal = toEnumWithDefaults bottom top intRp.newVal

class Random a <= RandomR a where
  randomR :: a -> a -> Seed -> RandomPair a

instance randomRInt :: RandomR Int where
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal `mod` (max - min + 1) + min

instance randomRNumber :: RandomR Number where
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal * (max - min) + min

instance randomRBoolean :: RandomR Boolean where
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal && max || min

instance randomRChar :: RandomR Char where
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: intRp.newSeed }
      where
        intRp = randomR (fromEnum min) (fromEnum max) seed
        newVal = toEnumWithDefaults bottom top intRp.newVal

-- Randoms

randoms :: forall a. Random a => Int -> Seed -> Array a
randoms i = randomsF random i

randomRs :: forall a. RandomR a => a -> a -> Int -> Seed -> Array a
randomRs min max = randomsF (randomR min max)

randomsF :: forall a. Random a => (Seed -> RandomPair a) -> Int -> Seed -> Array a
randomsF f i seed = run (withArray fill [])
  where
    fill :: forall h. STArray h a -> ST h Unit
    fill arr = do
      seedref <- new seed
      for 0 i \_ -> do
        seed' <- read seedref
        let rp = f seed'
        void (write rp.newSeed seedref)
        void (push rp.newVal arr)

-- RandomEff

randomEff :: forall a. Random a => Effect a
randomEff = pure <<< _.newVal <<< random =<< randomSeed

randomREff :: forall a. RandomR a => a -> a -> Effect a
randomREff min max = pure <<< _.newVal <<< randomR min max =<< randomSeed
