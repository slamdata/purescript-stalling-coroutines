## Module Control.Coroutine.Stalling

#### `StallF`

``` purescript
data StallF a b
  = Emit a b
  | Stall b
```

##### Instances
``` purescript
instance bifunctorStallF :: Bifunctor StallF
instance functorStallF :: Functor (StallF a)
```

#### `StallingProducer`

``` purescript
type StallingProducer o = Co (StallF o)
```

#### `StallingProcess`

``` purescript
type StallingProcess = Co Maybe
```

#### `fuse`

``` purescript
fuse :: forall o m a. (MonadRec m) => StallingProducer o m a -> Consumer o m a -> StallingProcess m a
```

#### `($$?)`

``` purescript
($$?) :: forall o m a. (MonadRec m) => StallingProducer o m a -> Consumer o m a -> StallingProcess m a
```

_left-associative / precedence -1_

#### `runStallingProcess`

``` purescript
runStallingProcess :: forall m a. (MonadRec m) => StallingProcess m a -> m (Maybe a)
```

#### `producerToStallingProducer`

``` purescript
producerToStallingProducer :: forall o m a. (Functor m) => Producer o m a -> StallingProducer o m a
```

#### `processToStallingProcess`

``` purescript
processToStallingProcess :: forall m a. (Functor m) => Process m a -> StallingProcess m a
```

#### `mapMaybe`

``` purescript
mapMaybe :: forall i o m a. (Functor m) => (i -> Maybe o) -> StallingProducer i m a -> StallingProducer o m a
```

Simultaneously map and filter a `StallingProducer`.

#### `filter`

``` purescript
filter :: forall o m a. (Functor m) => (o -> Boolean) -> StallingProducer o m a -> StallingProducer o m a
```

Filter a `StallingProducer`.


