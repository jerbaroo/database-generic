# database-generic

Database access (local or over network) without boilerplate, via `Generic`.

Summary of functionality:
 - Derive `Entity` instances to enable conversion to/from SQL.
 - Generate "create table" SQL commands for `Entity` types.
 - Interact with `Entity` in any HDBC-supported database via `MonadDb`.
 - Get a `servant` server for free, serving directly from your database!
 - `MonadDb` offer both singular value and streaming variants (via `Conduit`)!
 
## Quick start:

Full working code example if you know what you're doing. To get this code
working locally ASAP clone the `example` subdirectory of this repo, and then
either use nix-shell and cabal, or stack.

``` hs
TODO
```

## Introduction

At the heart of this library is a typeclass called `MonadDb` which provides some
functions for interacting with a database. With simplified type signatures these
functions look like:

``` hs
upsert :: a -> m ()
select :: b -> m (Maybe a)
-- etc..
```

In order to send your data types through the `MonadDb` functions your data type
needs to A. derive `Generic` and B. implement `Entity`. This is as simple as:

``` hs
data Person = Person { name :: String, age :: Int } deriving Generic

-- | Choosing 'name' as our primary key.
instance Entity Person "name"
```

Now you can send your `Person` through `MonadDb` functions!

``` hs
upsert $ Person "John" 21
print =<< select "John"
```

## Entity

Writing an instance of `Entity` is really as easy as we showed above. The
important part of writing an `instance Entity` is that you are defining a
primary key for your data type, represented by the `f` parameter:

``` hs
class Entity a f | a -> f where
  primaryKey :: forall b. (ToSqlValue b, HasField f a b) => a -> b
  primaryKey = getField @f
```

The `Entity` typeclass contains a few other functions, but they all have
`default` implementations. You can override them if you want but it's not
necessary. All the functions within `Entity` are used to convert to/from SQL.

## MonadDb

`upsert` and `select` have more complicated type signatures than we let on. For
example here is the full type signature of `upsertT`
