module Control.Coroutine.Stalling
  ( StallingProducer
  , StallingProcess
  , emit
  , stall
  , StallF(..)
  , stallF
  , producerToStallingProducer
  , processToStallingProcess
  , runStallingProcess

  , fuse
  , ($$?)

  , mapStallingProducer
  , catMaybes
  , filter
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Free.Trans as FT
import Control.Monad.Maybe.Trans as MT
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans as TR
import Control.Parallel.Class (class MonadPar)
import Control.Plus as P

import Data.Bifunctor as B
import Data.Either as E
import Data.Identity as I
import Data.Maybe as M

data StallF a b
  = Emit a b
  | Stall b

-- | The induction principle for `StallF`.
stallF
  :: forall a b r
   . (a -> b -> r)
  -> (b -> r)
  -> StallF a b
  -> r
stallF e s = case _ of
  Emit a b -> e a b
  Stall b -> s b

instance bifunctorStallF :: B.Bifunctor StallF where
  bimap f g = case _ of
    Emit a b -> Emit (f a) (g b)
    Stall b -> Stall (g b)

instance functorStallF :: Functor (StallF a) where
  map f = B.rmap f

type StallingProducer o = CR.Co (StallF o)
type StallingProcess = CR.Co M.Maybe

emit
  :: forall m o
   . Monad m
  => o
  -> StallingProducer o m Unit
emit =
  FT.liftFreeT
    <<< flip Emit unit

stall
  :: forall m o
   . Monad m
  => StallingProducer o m Unit
stall =
  FT.liftFreeT (Stall unit)

-- Fuse a `StallingProducer` with a `Consumer`.
fuse
  :: forall o m a
   . (MonadRec m, MonadPar m)
  => StallingProducer o m a
  -> CR.Consumer o m a
  -> StallingProcess m a
fuse =
  CR.fuseWith \f q (CR.Await g) ->
    case q of
      Emit o a -> M.Just (f a (g o))
      Stall _ -> M.Nothing

infix 4 fuse as $$?

runStallingProcess
  :: forall m a
   . MonadRec m
  => StallingProcess m a
  -> m (M.Maybe a)
runStallingProcess =
  MT.runMaybeT
    <<< FT.runFreeT (M.maybe P.empty pure)
    <<< FT.hoistFreeT (MT.MaybeT <<< map M.Just)

producerToStallingProducer
  :: forall o m a
   . Functor m
  => CR.Producer o m a
  -> StallingProducer o m a
producerToStallingProducer =
  FT.interpret \(CR.Emit o a) ->
    Emit o a

processToStallingProcess
  :: forall m a
   . Functor m
  => CR.Process m a
  -> StallingProcess m a
processToStallingProcess =
  FT.interpret
    (M.Just <<< I.runIdentity)

mapStallingProducer
  :: forall i o m a
   . Functor m
  => (i -> o)
  -> StallingProducer i m a
  -> StallingProducer o m a
mapStallingProducer =
  FT.interpret <<< B.lmap

catMaybes
  :: forall o m a
   . MonadRec m
  => StallingProducer (M.Maybe o) m a
  -> StallingProducer o m a
catMaybes =
  tailRecM $
    FT.resume >>> TR.lift >=>
      E.either
        (E.Right >>> pure)
        (stallF
          (\mo t -> M.maybe (pure unit) emit mo $> E.Left t)
          (E.Left >>> pure))

filter
  :: forall o m a
   . MonadRec m
  => (o -> Boolean)
  -> StallingProducer o m a
  -> StallingProducer o m a
filter p =
  catMaybes <<< mapStallingProducer \x ->
    if p x then M.Just x else M.Nothing
