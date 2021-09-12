# purescript-pseudo-random

[![Build status](https://travis-ci.org/opyapeus/purescript-pseudo-random.svg?branch=master)](https://travis-ci.org/opyapeus/purescript-pseudo-random)

A pseudo random value generator like [System.Random](http://hackage.haskell.org/package/random) in Haskell.

This library depends on [purescript-lcg](https://github.com/purescript/purescript-lcg) using linear congruential generator algorithm (LCG).

## Examples

```PureScript
> import Random.PseudoRandom
> randomRs 0 10 5 (mkSeed 1)
[3,8,1,5,9]
> randomRs 0.0 10.0 5 (mkSeed 1)
[0.00022477936010098986,0.8503244914348818,6.0135260531741785,8.916112770753035,9.679557019695434]
> randomRs true false 5 (mkSeed 1)
[false,false,true,true,true]
> randomRs 'a' 'z' 5 (mkSeed 1)
['p','u','c','l','h']
```

## Installation

```sh
bower install purescript-pseudo-random
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-pseudo-random).
