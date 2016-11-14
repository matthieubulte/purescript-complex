module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.Data.Complex (testComplex)

main :: forall eff. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) Unit
main = testComplex
