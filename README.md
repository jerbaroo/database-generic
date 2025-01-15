# database-generic

Database access (local or over network) with minimal boilerplate.

Summary of functionality:
 - Derive (schemas) instances of `Entity` for your data types.
 - Persist your instances of `Entity` via `MonadDb` operations.
 - Serve your instances of `Entity` via a provided `servant` server.
 - Stream your instances of `Entity` from the database via `Conduit`.
 - Stream your instances of `Entity` over the network via WebSockets.
 
## Quick Start

To get [THIS EXAMPLE] running on your machine:
1. Clone this repo.
2. Start a PostgreSQL instance with username and password `"demo"`, e.g.:
  `docker run -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish 5432:5432 postgres`
3. Either `cabal sun` via provided `nix-shell`, or `stack run`.

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

An instance of `Entity` may be derived via `DeriveAnyClass`: 

``` hs
data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic)
```

The `Entity ` class has three superclasses:
- `FromSqlValues`: for parsing from SQL
- `HasSqlFields`: names and types of SQL columns
- `ToSqlValues`: converting to SQL

In addition, the `Entity` class has two methods with `default` implementations:
- `entityName`: name of the SQL table
- `primaryKeyField`: field used as primary key

When `deriving` an instance of `Entity` you can decide the primary key by
passing the field `f` to use as primary key as first type parameter to `Entity`.

IMPORTANT NOTE: the implementations of the three superclasses and the two
methods are consistent with each other. If you override any of these instances
or implementations then you need to ensure they are still consistent.

## MonadDb

`upsert` and `select` have more complicated type signatures than shown above.
For example here is the full type signature of `upsertT`
