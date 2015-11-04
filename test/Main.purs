module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor (($>))

import Control.Coroutine as CR
import Control.Coroutine.Stalling as SCR
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Rec.Class as MR

nats :: forall m. (Monad m) => SCR.StallingProducer Int m Unit
nats = go 0
  where
    go i = do
      SCR.emit i
      go (i + 1)

evens :: forall m. (MR.MonadRec m) => SCR.StallingProducer Int m Unit
evens = SCR.filter (\x -> x `mod` 2 == 0) nats

printer :: forall e. CR.Consumer String (Eff (console :: CONSOLE | e)) Unit
printer =
  CR.consumer \s ->
    log s $> Nothing

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  SCR.runStallingProcess (SCR.mapStallingProducer show evens SCR.$$? printer)
  pure unit
