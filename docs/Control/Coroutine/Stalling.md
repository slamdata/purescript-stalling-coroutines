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

#### `stallF`

``` purescript
stallF :: forall a b r. (a -> b -> r) -> (b -> r) -> StallF a b -> r
```

#### `StallingProducer`

``` purescript
type StallingProducer o = Co (StallF o)
```

#### `StallingProcess`

``` purescript
type StallingProcess = Co Maybe
```

#### `emit`

``` purescript
emit :: forall m o. (Monad m) => o -> StallingProducer o m Unit
```

#### `stall`

``` purescript
stall :: forall m o. (Monad m) => StallingProducer o m Unit
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

#### `mapStallingProducer`

``` purescript
mapStallingProducer :: forall i o m a. (Functor m) => (i -> o) -> StallingProducer i m a -> StallingProducer o m a
```

#### `catMaybes`

``` purescript
catMaybes :: forall o m a. (MonadRec m) => StallingProducer (Maybe o) m a -> StallingProducer o m a
```

#### `filter`

``` purescript
filter :: forall o m a. (MonadRec m) => (o -> Boolean) -> StallingProducer o m a -> StallingProducer o m a
```


