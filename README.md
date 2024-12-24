# database-generic

Database access (local or over network) with minimal boilerplate, via `Generic`.

Summary of functionality:
 - Derive `Entity` instances to enable conversion to/from SQL.
 - Persist `Entity` in any supported database via `MonadDb`.
 - Get a `servant` server for free, serving directly from your database!
 - `MonadDb` offers both singular value and streaming variants (via `Conduit`)!
 - Use `MonadDb` in your web app, including streaming directly from database! 
 
## Quick Start

Full working code example [HERE]. To get it running locally ASAP:
1. clone the `demo` subdirectory of this repo
2. Start a PostgreSQL instance with username and password `"demo"`, e.g.:
  `docker run -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish 5432:5432 postgres`
3. use `cabal` via provided `nix-shell`, or use `stack` to run the executable

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
