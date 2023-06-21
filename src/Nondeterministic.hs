{-|
Module      : Nondeterministic
Description : Nondeterministic orbit-finite automata

Some of the definitions in this module are inspired by the `nlambda` library.
-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RankNTypes #-}
module Nondeterministic where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)
import GHC.Generics (Generic)


-- | Nondeterministic orbit-finite automaton without initial state, with state space an (orbit-finite) set of elements of Nominal type _q_. See
data NOFACoalg q a = NOFACoalg {stateSpace :: Set q, inputSymbols :: Set a, transition :: q -> a -> Set q, acceptStates :: Set q}
  deriving (Generic, Contextual, Conditional)


-- -- | Converts a coalgebraic DA _aut_ into the nlambda DA with initial state _i_
-- nofaCoalgtoNL :: Nominal q =>  NOFACoalg q -> Set q -> Automaton q Atom
-- nofaCoalgtoNL aut i = atomsNA (stateSpace aut) d i (acceptStates aut)
--   where d = pairsWith (\s l -> (s,l,(transition aut) s l)) (stateSpace aut) atoms


-- | Transitions from state `s` in automaton `aut` with input word `w`. Note there are multiple reachable states in general.
nofaCoalgTransit :: (Nominal q, Nominal a) => NOFACoalg q a -> q -> [a] -> Set q
nofaCoalgTransit aut s =
  foldl (\s' a -> sum (map (\s'' -> transition aut s'' a) s')) (singleton s)


-- | Whether a state _s_ in coalgebraic DA _q_ accepts the input string of atoms _s_. It accepts if any of the states reached this way is an accepting state
nofaCoalgAccepts :: (Nominal q, Nominal a) => NOFACoalg q a -> q -> [a] -> Formula
nofaCoalgAccepts aut s w =
  acceptStates aut `intersect` s' where
  s' = nofaCoalgTransit aut s w