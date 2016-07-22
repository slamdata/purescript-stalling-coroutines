module Test.Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Stalling (($$?))
import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Array (snoc)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Test.Assert (ASSERT, assert)

nats :: forall m. Monad m => SCR.StallingProducer Int m Unit
nats = go 1
  where
    go i
      | i > 10 = pure unit
      | otherwise = do
          SCR.emit i
          go (i + 1)

evens :: forall m. MonadRec m => SCR.StallingProducer Int m Unit
evens = SCR.filter (\x -> x `mod` 2 == 0) nats

accum :: forall e. Ref (Array Int) -> CR.Consumer Int (Aff (console :: CONSOLE, ref :: REF | e)) Unit
accum ref = CR.consumer \i -> liftEff $ modifyRef ref (_ `snoc` i) $> Nothing

main :: forall e. Eff (console :: CONSOLE, ref :: REF, assert :: ASSERT, err :: EXCEPTION | e) Unit
main = void $ launchAff do
  ref <- liftEff $ newRef []
  void $ SCR.runStallingProcess $ evens $$? accum ref
  evenNats <- liftEff $ readRef ref
  liftEff $ assert $ evenNats == [2, 4, 6, 8, 10]
