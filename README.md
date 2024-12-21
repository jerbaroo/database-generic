# database-generic

Database access (local or over network) without boilerplate, via `Generic`.

Summary of functionality:
 - Derive `Entity` instances to enable conversion to/from SQL.
 - Interact with `Entity` in any HDBC-supported database via `MonadDb`.
 - Get a `servant` server for free, serving directly from your database!
 - `MonadDb` offers both singular value and streaming variants (via `Conduit`)!
 
## Quick Start

Full working code example if you know what you're doing. To get this code
working locally ASAP clone the `example` subdirectory of this repo, and then
either use nix-shell and cabal, or stack.

``` hs
TODO
```

## Introduction

At the heart of this library is a typeclass called `MonadDb` which provides some
class methods for interacting with a database. Simplifying the type signatures a
bit, these functions look like:

``` hs
upsert :: a -> m ()
select :: b -> m (Maybe a)
-- etc.
```

In order to send data types through the `MonadDb` methods we only need to derive
`Entity` and `Generic`. This is as simple as:

``` hs
data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic) -- Choosing "name" as primary key.
```

Now `MonadDb` will accept the `Person` data type!

``` hs
upsert $ Person "John" 21
print =<< select "John"
```

## Entity

Deriving an instance of `Entity` is really as easy as we showed above. The
important reason we need to write an `instance Entity` is to choose a primary
key for our data type, represented by the `f` parameter:

``` hs
class Entity f a | a -> f where
  primaryKey :: forall b. (ToSqlValue b, HasField f a b) => a -> b
  primaryKey = getField @f
```

The `Entity` typeclass contains a few other functions, but they all have
`default` implementations. You can override them if you want but it's not
necessary. All the functions within `Entity` are used to convert to/from SQL.

## MonadDb

`upsert` and `select` have more complicated type signatures than shown above.
For example here is the full type signature of `upsertT`
