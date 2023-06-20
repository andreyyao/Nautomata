{-# LANGUAGE RankNTypes #-}
module Reduction where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)
import qualified Deterministic as DA


-- | Reduction for deterministic orbit-finite automata, without considering the initial state. Requires the alphabet to be the set of atoms. Returns a new DA where behaviorally equivalent states are all identified.
daReduction :: (Nominal q) => DA.DACoalg q -> DA.DACoalg q
daReduction nautomaton = nautomaton