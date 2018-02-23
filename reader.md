# The Reader Monad, MonadReader Class, and ReaderT Transformer

Some common and useful monad typeclasses and their instances are defined in mtl.
eg., MonadReader class, MonadState class.

Concrete monad transformers are defined in transformers.
eg., ReaderT, StateT.

Reader monad transformer (ReaderT)
as instances of different monads are defined separately,

|      mtl      | transformers             |
| ------------- | ------------------------ |
|  MonadReader (ask, local, reader)  | type declaration         |
|               | Monad ((>>=), (>>), return, fail) |
|               | MonadTrans (lift) |
|               | Functor (fmap, (<\$))   |
|               | Applicative (pure, (<*>), (<*), (*>)) |
|               | MonadIO (liftIO) |
|               | Alternative (empty, some, many, (<&#124>))|
|               | MonadPlus (mzero, mplus)              |
|               | MonadFix (mFix)                       |
|               | MonadZip (mzip, mzipWith, munzip)     |
|               | MonadFail (fail)                      |


## MonadReader

Typeclass **MonadReader** defines the reader behaviors.

```haskell
-- mtl-2.2.1

class Monad m => MonadReader r m | m -> r where
  -- | Retrieves the monad environment.
  ask :: m r
  ask = reader id

  -- | Executes a computation in a modified environment.
  local :: (r -> r) -> m a -> m a

  -- | Retrieves a function of the current environment.
  reader :: (r -> a) -> m a
  reader f = do
    r <- ask
    return (f r)

  -- if we rewrite do notation with >>=:
  -- reader f = ask >>= (\r -> return $ f r)
```

* `ask` and `reader` can be deduced from each other.
  At least one needs to be defined.

* `local` "inserts" `f :: r -> r` ahead of the original computation
  on the environment.


### Instances

#### ((->) r)

A simple instance of typeclass **MonadReader** is **((->) r)**

```haskell
-- mtl-2.2.1

instance MonadReader r ((->) r) where
  -- ask :: (->) r r
  ask = id

  -- local :: (r -> r) -> ((->) r a) -> ((->) r a)
  local f m = m . f

  -- reader :: (r -> a) -> ((->) r a)
  reader = id
```

Example usage

```haskell
foo :: Int -> Int;
foo = (+) 1

bar :: Int -> Int
bar = (*) 2

fr :: Int -> Int
fr = local bar $ reader foo

-- fr = foo . bar
-- fr 5 = 11
```

#### Reader

Another instance of typeclass **MonadReader** is **Reader**, which is
defined in transformers package.

**Reader** is defined as a type alias of **ReaderT r Identity**,
there is such thing as the value constructor defined for **Reader** type,
but the `reader` function can be roughly viewed as its constructor,
when combined with the `runReader` function. `reader` wraps
result of a function into an **Identity** monad, and `runReader` unwraps
the result of the function from **Identity** monad by applying `runIdentity`.

```haskell
-- transformers-0.5.5.0

type Reader = ReaderT r Identity
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- mtl-2.2.1

type Reader r = ReaderT * r Identity
newtype ReaderT k r m a :: forall k. * -> (k -> *) -> k -> *
```

**Reader** as instance of **MonadReader**

```haskell
-- mtl-2.2.1

instance MonadReader r (ReaderT r m) where
  ask = ReaderT.ask
  local = ReaderT.local
  reader = ReaderT.reader
```

Further definitions are defined in transformers package
```haskell
-- transformers-0.5.5.0

-- | Fetch the value of the environment.
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

-- | Execute a computation in a modified environment
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

-- | Execute a computation in a modified environment
-- runReaderT (withReaderT f m) = runReaderT m . f
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

-- | Constructor for computations in the reader monad (equivalent to 'asks').
reader :: (Monad m) => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)
```

Now let us simply the definitions for **Reader**

```haskell
instance MonadReader r (Reader r) where

  ask :: Reader r r
  ask = ReaderT Identity
  -- ask = ReaderT return
  -- and since reader f = ReaderT (return . f)
  ask = reader id

  local :: (r -> r) -> Reader r a -> Reader r a
  local = withReader

  withReader :: (r' -> r) -> Reader r a -> Reader r' a
  -- withReader f m = ReaderT $ runReaderT m . f
  -- runReader m = runIdentity . runReaderT m
  -- reader f = ReaderT (return . f)
  withReader f m = reader $ runReader m . f

  reader :: (r -> a) -> Reader r a
  -- reader f = ReaderT (return . f)
  reader f = ReaderT (Identity . f)
  -- `reader` converts `f` into a reader monad.
```

Notice that `ReaderT $ runReaderT m . f` is equal to `reader $ runReader m . f`
when **ReaderT** is applied on base monad **Identity**, ie, `m :: Reader r a`
and type of the expression is `Reader r' a`

```haskell
-- pseudo code
reader $ runReader m . f
= ReaderT (return . runReader m . f)
= ReaderT (Identity . runIdentity . runReaderT m . f)
= ReaderT (runReaderT m . f)
```

## Reader As The Monad

#### Definition

```haskell
-- transformers-0.5.5.0

instance (Monad m) => Monad (ReaderT r m) where
  return  = lift . return
  -- lift m = ReaderT (const m)
  -- const ignores the value of the environment
  -- the computation wrapped in the new monad is basically, \ _ -> ..

  m >>= k = ReaderT $ \ r -> do
    a <- runReaderT m r
    runReaderT (k a) r
```

Let's simply for **Reader**

```haskell
instance Monad (Reader r) where
  return = reader . const

  m >>= f = reader $ \ r -> runReader (f $ runReader m r) r
```

The simplified `>>=` is from
```haskell
m >>= f = let f = \ r -> (runReaderT m r) >>= (\ x -> runReaderT (f x) r)
          in ReaderT f
-- for Identity monad, m >>= k  = k (runIdentity m)
m >>= f = let f = \ r -> (\ x -> runReaderT (f x) r) (runIdentity $ (runReaderT m) r)
          in ReaderT f
-- runReader m = runIdentity . runReaderT m
m >>= f = let f = \ r -> (\ x -> runReaderT (f x) r) (runReader m r)
          in ReaderT f
-- function application
m >>= f = let f = \ r -> runReaderT (f (runReader m r)) r
          in ReaderT f
-- apply Identity and its inverse runIdentity
m >>= f = ReaderT $ Identity . (\ r -> runIdentity $ runReaderT (f (runReader m r)) r)
-- reader f = ReaderT $ Identity . f
m >>= f = reader (\ r -> (runIdentity . (runReaderT (f (runReader m r)))) r)
-- runReader m = runIdentity . runReaderT m
m >>= f = reader $ \ r -> runReader (f $ runReader m r) r
```

#### Decipher `do` Notation of Reader

Inside of the `do` notation for **Reader** monad,
`a <- m` where `m` is a **Reader** monad, then `a`
can be view as the computed result based on the
enclosed environment of the reader.

New readers are made from "appending" or "manipulating"
computation of the original reader. Intuitively,
"actions" on reader monads, create new computations on
the same context (the enclosed environment).

A simple example is,

```haskell
appendEnv :: a -> Reader r (a, r)
appendEnv x = do
  e <- ask
  return (x, e)
```

`appendEnv` is equivalent to

```haskell
appendEnv :: a -> Reader r (a, r)
appendEnv x = (reader id) >>= (\ e -> return (x, e))
```

apply definition of `>>=` of `Reader`
```haskell
-- m >>= f = reader $ \ r -> runReader (f $ runReader m r) r
appendEnv x = let m = reader id
                  f = \ e -> reader $ const (x, e)
              in reader $ \ r -> runReader (f $ runReader m r) r
appendEnv x = reader $ \ r -> runReader ((\ e -> reader $ const (x, e)) (runReader (reader id)) r) r
appendEnv x = reader $ \ r -> runReader (reader $ const (x, (runReader (reader id)) r)) r
appendEnv x = reader $ \ r -> runReader (reader $ const (x, r)) r
appendEnv x = reader $ \ r -> const (x, r) r
appendEnv x = reader $ \ r -> (x, r)
appendEnv x = reader $ (,) x
appendEnv = reader . (,)
```
In the above example, view `e <- ask` as apply `id` on environment and let `e`
= `id` environment.

