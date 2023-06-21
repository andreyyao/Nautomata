{-|
Module      : Reduction
Description : Reduction Algorithms for coalgebras

Some of the definitions in this module are inspired by the `nlambda` library.
-}
{-# LANGUAGE RankNTypes #-}
module Reduction where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)
import qualified Deterministic as DA


-- -- | Reduction for deterministic orbit-finite automata, without considering the initial state. Requires the alphabet to be the set of atoms. Returns a new DA where behaviorally equivalent states are all identified. Uses the Hopcroft partition-refinement algorithm.
-- daReduction :: (Nominal q, Nominal a) => DA.DACoalg q a -> DA.DACoalg (Set q) a
-- daReduction aut = DA.DACoalg s' d' f' where
--   f = DA.acceptStates aut
--   nf = DA.stateSpace aut \\ f -- nonfinal states
--   p = fromList [f, nf] -- initial partition
--   a = atom "a" -- atom "a"
--   refine s'' a'' = partition
