# database-generic

Database-agnostic interface to generically persisted data.

## Introduction

Explanation of the above:
- Database-agnostic interface: the typeclass is called `MonadDb`, and you must
  specify how it can communicate with your database (we provide an example for
  connecting to a PostgreSQL server).
- Generically persisted data: you can derive the necessary instances for your
  data types via `Generics`. This will enable `MonadDb` to read/write instances
  of your data types to/from your database.

A key intended feature of this library is that the typeclass `MonadDb` can be
used either server-side or client-side. Allowing your client application (e.g.
web app) to use the same functions to access your data as your server-side does.

Another important intended feature is an optional `servant` server. Merely
provide an instance of `MonadDb` so the server knows how to communicate with
your database, then the server can act as a proxy to allow clients to read/write
to your database without having to write the usual server boilerplate.

## Quick Start

A tutorial as code exists [here](tutorial/tutorial/Main.hs).

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
