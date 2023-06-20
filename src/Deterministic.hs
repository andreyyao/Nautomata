{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RankNTypes #-}
module Deterministic where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)
import GHC.Generics (Generic)


-- | Nominal Deterministic automaton without initial state, with state space an (orbit-finite) set of elements of Nominal type _q_. This definition is more natural in the coalgebraic setting. Type definition modified from the _Automaton_ type in nlambda. Invariant: transition is a function: No two triples have the same first two entries.
data DACoalg q = DACoalg {stateSpace :: Set q, transition :: q -> Atom -> q, acceptStates :: Set q}
  deriving (Generic, Contextual, Conditional)


-- | Converts a coalgebraic DA _aut_ into the nlambda DA with initial state _i_
daCoalgtoNL :: Nominal q =>  DACoalg q -> q -> Automaton q Atom
daCoalgtoNL aut i = da (stateSpace aut) atoms (transition aut) i (acceptStates aut)


-- | Transitions from state `s` in automaton `aut` with input word `w`
daCoalgTransit :: Nominal q => DACoalg q -> q -> [Atom] -> q
daCoalgTransit aut = foldl (transition aut)


-- | Whether a state _s_ in coalgebraic DA _q_ accepts the input string of atoms _s_
daCoalgAccepts :: Nominal q => DACoalg q -> q -> [Atom] -> Formula
daCoalgAccepts aut s w =
  contains (acceptStates aut) s' where
  s' = daCoalgTransit aut s w