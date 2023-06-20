{-# LANGUAGE MultiParamTypeClasses #-}
module Coalgebra where

import NLambda
import Prelude hiding (or, and, not, sum, map, filter, maybe)


-- | Functors in Nom
class (Nominal a, Nominal b) => NFunctor a b f where
  fmap :: (a -> b) -> f a -> f b

-- The type class for Nominal coalgebras
-- class NFunctor f => Coalg f where
