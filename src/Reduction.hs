{-# LANGUAGE RankNTypes #-}
module Reduction where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)


-- reduction for deterministic orbit-finite automata, without considering the initial state. Requires the alphabet to be the set of atoms
daReduction :: (Nominal q) => Automaton q Atom -> Automaton q Atom
daReduction nautomaton = nautomaton