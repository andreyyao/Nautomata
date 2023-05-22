{-# language RankNTypes #-}

module Main where

import NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import GHC.Generics (Generic)
import Prelude (Eq, Ord, Show, ($), IO, String, putStr, Int)
import qualified Prelude ()

type T = Nominal
x = NLambda.alphabet

main :: IO ()
main = putStr "Hehe"