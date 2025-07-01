# database-generic

Easy way to start serving your Haskell data types from your database.

Intended for:
- rapid prototyping
- internal dashboards

Not (yet) intended for:
- public-facing production apps
- apps requiring complex database queries

## Introduction

This library provides a database-agnostic interface to generically persisted
data.

Explanation:
- Database-agnostic: this library exports a typeclass called `MonadDb` for
  communicating with a database of your choice, so you will need to write an
  `instance MonadDb MyApp` to specify how to communicate with your database. We
  provide an example for Postgres in the [tutorial](tutorial/tutorial/Main.hs).
- Generically persisted data: it is easy to derive all the necessary instances
  for your data types in one line of code. These instances will allow `MonadDb`
  to read/write your data to/from your database.

Two important features of this library:
- **Server for free**: merely provide an instance of `MonadDb` so the server
  knows how to communicate with your database. Avoid needing to write the usual
  server boilerplate!
- **Same code server-side and client-side**: your client application (e.g. web
  app) can use the same functions to access your data as your server-side does!

## Quick Start

The [tutorial](tutorial/tutorial/Main.hs) is the recommended way of becoming
familiar with `database-generic`.

To run the tutorial on your machine:
1. Clone this repo.
2. Install deps. Easiest is to use Nix shell: `nix-shell`
3. Start a Postgres server with username and password `demo`, e.g.: `docker run
  -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish
  5432:5432 postgres`
4. Run the tutorial: `cabal run tutorial`

## Features

Working examples of the following features can be found in the
[tutorial](tutorial/tutorial/Main.hs).

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

The following _attempts_ to descibe how this library works. However it is
recommended to read through the [tutorial](tutorial/tutorial/Main.hs) first, and
only come back and read this if you feel you need to.

### Necessary Instances

Since `Person` meets all of the critera, a few type classes will be
automatically derived for `Person` via `Generic`:
- `HasDbColumns Person`: names & types of database fields/columns
- `HasEntityName Person`: a name for the database collection/table
- `FromDbValues Person`: to convert from database values
- `ToDbValues Person`: to convert to database values

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
