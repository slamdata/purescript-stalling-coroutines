module Control.Coroutine.Stalling
  ( StallF(..)
  , StallingProducer()
  , StallingProcess()
  , producerToStallingProducer
  , processToStallingProcess
  , runStallingProcess

  , ($$?)

  , mapStallingProducer
  , catMaybes
  , filter
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Free.Trans as FT
import Control.Monad.Maybe.Trans as MT
import Control.Monad.Rec.Class as MR
import Control.Plus as P
import Data.Bifunctor as B
import Data.Identity as I
import Data.Maybe as M
import Data.Tuple as T

data StallF a b
  = Emit a b
  | Stall b

instance bifunctorStallF :: B.Bifunctor StallF where
  bimap f g q =
    case q of
      Emit a b -> Emit (f a) (g b)
      Stall b -> Stall (g b)

instance functorStallF :: Functor (StallF a) where
  map f = B.rmap f

type StallingProducer o = CR.Co (StallF o)
type StallingProcess = CR.Co M.Maybe

($$?)
  :: forall o m a
   . (MR.MonadRec m)
  => StallingProducer o m a
  -> CR.Consumer o m a
  -> StallingProcess m a
($$?) =
  CR.fuseWith \f q (CR.Await g) ->
    case q of
      Emit o a -> M.Just (f a (g o))
      Stall _ -> M.Nothing

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce ""

runStallingProcess
  :: forall m a
   . (MR.MonadRec m)
  => StallingProcess m a
  -> m (M.Maybe a)
runStallingProcess =
  MT.runMaybeT
    <<< FT.runFreeT (M.maybe P.empty pure)
    <<< FT.hoistFreeT (MT.MaybeT <<< map M.Just)

producerToStallingProducer
  :: forall o m a
   . (Functor m)
  => CR.Producer o m a
  -> StallingProducer o m a
producerToStallingProducer =
  FT.interpret \(CR.Emit o a) ->
    Emit o a

processToStallingProcess
  :: forall m a
   . (Functor m)
  => CR.Process m a
  -> StallingProcess m a
processToStallingProcess =
  FT.interpret
    (M.Just <<< I.runIdentity)

mapStallingProducer
  :: forall i o m a
   . (Functor m)
  => (i -> o)
  -> StallingProducer i m a
  -> StallingProducer o m a
mapStallingProducer =
  FT.interpret <<< B.lmap

catMaybes
  :: forall o m a
   . (Functor m)
  => StallingProducer (M.Maybe o) m a
  -> StallingProducer o m a
catMaybes =
  FT.interpret \q ->
    case q of
      Emit i a -> M.maybe (Stall a) (flip Emit a) i
      Stall a -> Stall a

filter
  :: forall o m a
   . (Functor m)
  => (o -> Boolean)
  -> StallingProducer o m a
  -> StallingProducer o m a
filter p =
  catMaybes <<< mapStallingProducer \x ->
    if p x then M.Just x else M.Nothing
