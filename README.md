# database-generic

Database-agnostic interface to generically persisted data.

## Introduction

Explanation of the above:
- Database-agnostic: the typeclass is called `MonadDb`, and you must specify how
  an instance can communicate with your database. We provide an example for
  connecting to Postgres in the [runnable tutorial](tutorial/tutorial/Main.hs).
- Generically persisted data: you can derive the necessary instances in one line
  via `Generics`, to enable `MonadDb` to read/write instances of your data types
  to/from your database.

A key intended feature of this library is that the typeclass `MonadDb` can be
used either server-side or client-side. Allowing your client application (e.g.
web app) to use the same functions to access your data as your server-side does.

Another important intended feature is an optional `servant` server. Merely
provide an instance of `MonadDb` so the server knows how to communicate with
your database, then the server can act as a proxy to allow clients to read/write
to your database without having to write the usual server boilerplate.

## Quick Start

The [runnable tutorial](tutorial/tutorial/Main.hs) is the recommended way of
becoming familiar with `database-generic`.

To run the tutorial on your machine:
1. Clone this repo.
2. Start a PostgreSQL instance with username and password `demo`, e.g.:
  `docker run -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish 5432:5432 postgres`
3. Then `cabal run tutorial` via provided `nix-shell`.

## Features

Examples of the following features can be found in [the
tutorial](tutorial/tutorial/Main.hs).

| Feature                                  | In Tutorial | Tested |
|------------------------------------------|-------------|--------|
| Create table                             | ✅          | ✅     |
| Insert one                               | ✅          |        |
| Insert many                              | ✅          |        |
| Insert returning                         | ✅          |        |
| Insert returning fields                  | ✅          | ✅     |
| Select by PK                             | ✅          | ✅     |
| Select all                               | ✅          | ✅     |
| Select returning fields                  | ✅          | ✅     |
| Where column equals                      |             |        |
| Where column is null                     |             |        |
| Where column is not null                 |             |        |
| Limit clause                             | ✅          | ✅     |
| Offset clause                            | ✅          | ✅     |
| Order by clause                          | ✅          | ✅     |
| Delete by PK                             | ✅          | ✅     |
| Delete all                               | ✅          | ✅     |
| Delete returning                         | ✅          | ✅     |
| Server: endpoint to execute statement    | ✅          |        |
| Joins                                    |             |        |
| Stream statements over Conduit           |             |        |
| Stream updates over Conduit              |             |        |
| Server: stream statements over WebSocket |             |        |
| Server: stream updates over WebSocket    |             |        |
| Server: permission checks                |             |        |
| Reflex (client-side) MonadDb instance    |             |        |

## Documentation

The following attempts to descibe how the library works, and the various type
classes involved. However it is recommended to begin by reading through the code
in the [runnable tutorial](tutorial/tutorial/Main.hs), and only come back and
read this if you feel you need to.

### Necessary Instances

The easiest way to use a data type with this library is to:
- ensure the data type only has one constructor
- derive a `Generic` instance
- derive a `PrimaryKey` instance
- ensure each field has a `FromDbValues` instance (for reading)
- ensure each field has a `HasDbType` instance (for schema)
- ensure each field has a `ToDbValues` instance (for writing)

Here is a simple example which meets all of the criteria:
``` hs
data Person = Person { age :: !Int64, name :: !String, ownsDog :: !(Maybe Bool) }
  deriving (Generic, PrimaryKey "name")
```

Since `Person` meets all of the critera, a few type classes will be
automatically derived for `Person` via `Generic`:
- `HasDbColumns Person`: to provide a database schema
- `HasEntityName Person`: to provide a database schema
- `FromDbValues Person`: to convert from database values
- `ToDbValues Person`: to convert to database values

### HasDbColumns

`HasDbColumns` is responsible for converting each field into a named field along
with type information. Here is the definition of `HasDbColumns`:
``` hs
class HasDbColumns a where
  dbColumns :: [(FieldName, DbTypeN)]
```

For our `Person` data type, `dbColumns` will return:
``` hs
[ (FieldName "age"    , DbTypeN False (DbInt64  Unit))
, (FieldName "name"   , DbTypeN False (DbString Unit))
, (FieldName "ownsdog", DbTypeN True  (DbBool   Unit))
]
```

To customize column names just create an instance of `FieldNameTransformation`.
``` hs
instance FieldNameTransformation Person where
  fieldNameT = toSnakeCase
```

### HasEntityName

When using the provided `PostgreSQL` database type, a `createTable @Person`
statement will be translated to the following SQL:

``` sql
CREATE TABLE (age BIGINT NOT NULL, name VARCHAR NOT NULL PRIMARY KEY, ownsdog BOOLEAN);
```

### FromDBValues

Above we mentioned that each field of a data type needs to have a `FromDbValues`
instance. This is to ensure that each field can be parsed from a column value
read from the database.

However `FromDbValues dbv a` is actually a multi-parameter type class. When
using the provided `PostgreSQL` database the `dbv` will be instantiated to
the library provided `DbValueN` type.
