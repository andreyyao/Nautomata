{-|
Module      : Deterministic
Description : Deterministic orbit-finite automata

Some of the definitions in this module are inspired by the `nlambda` library.
-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RankNTypes #-}
module Deterministic where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)
import GHC.Generics (Generic)


-- | Nominal Deterministic automaton without initial state, with state space an (orbit-finite) set of elements of Nominal type _q_. This definition is more natural in the coalgebraic setting. Type definition modified from the _Automaton_ type in nlambda. Invariant: transition is a function: No two triples have the same first two entries.
data DACoalg q a = DACoalg {stateSpace :: Set q, inputSymbols :: Set a, transition :: q -> a -> q, acceptStates :: Set q}
  deriving (Generic, Contextual, Conditional)


-- | Converts a coalgebraic DA _aut_ into the nlambda DA with initial state _i_
daCoalgtoNL :: (Nominal q, Nominal a) =>  DACoalg q a -> q -> Automaton q a
daCoalgtoNL aut i = da (stateSpace aut) (inputSymbols aut) (transition aut) i (acceptStates aut)


-- | Transitions from state `s` in automaton `aut` with input word `w`
daCoalgTransit :: (Nominal q, Nominal a) => DACoalg q a -> q -> [a] -> q
daCoalgTransit aut = foldl (transition aut)


-- | Whether a state _s_ in coalgebraic DA _q_ accepts the input string of atoms _s_
daCoalgAccepts :: (Nominal q, Nominal a) => DACoalg q a -> q -> [a] -> Formula
daCoalgAccepts aut s w = contains (acceptStates aut) s' where
  s' = daCoalgTransit aut s w