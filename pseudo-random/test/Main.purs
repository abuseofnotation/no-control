module Test.Main where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Class.Console (log)
import Random.PseudoRandom (class Random, class RandomR, random, randomR, randomRs, randoms)
import Test.QuickCheck (Seed, mkSeed, quickCheck, (==?))

main :: Effect Unit
main = do
  let maxStack = 100000
  log "randomRs is stack-safe"
  let _ = randomRs 0 10 maxStack (mkSeed 1) :: Array Int

  log "randoms should equal to random chain"
  log "Int"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? (randoms' len seed :: Array Int)
  log "Number"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? (randoms' len seed :: Array Number)
  log "Boolean"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? (randoms' len seed :: Array Boolean)
  log "Char"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? (randoms' len seed :: Array Char)
  log "randomRs should equal to randomR chain"
  log "Int"
  quickCheck \n -> let seed = mkSeed n in randomRs 0 10 len seed ==? randomRs' 0 10 len seed
  log "Number"
  quickCheck \n -> let seed = mkSeed n in randomRs 0.0 10.0 len seed ==? randomRs' 0.0 10.0 len seed
  log "Boolean"
  quickCheck \n -> let seed = mkSeed n in randomRs true false len seed ==? randomRs' true false len seed
  log "Char"
  quickCheck \n -> let seed = mkSeed n in randomRs 'a' 'z' len seed ==? randomRs 'a' 'z' len seed
  where len = 10

randoms' :: forall a. Random a => Int -> Seed -> Array a
randoms' n seed = map _.newVal $ iterate next n initial
  where
    initial = random seed
    next rp = random rp.newSeed

randomRs' :: forall a. RandomR a => a -> a -> Int -> Seed -> Array a
randomRs' min max n seed = map _.newVal $ iterate next n initial
  where
    initial = randomR min max seed
    next rp = randomR min max rp.newSeed

-- NOTE: assumption that i is greater than 0
iterate :: forall a. (a -> a) -> Int -> a -> Array a
iterate f 0 x = []
iterate f n x = x : iterate f (n - 1) (f x)
