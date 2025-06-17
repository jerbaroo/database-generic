# database-generic

Database-agnostic interface to generically persisted data.

## Introduction

Explanation of the above:
- Database-agnostic: the typeclass is called `MonadDb`, and you must specify how
  an instance can communicate with your database. We provide an example for
  connecting to Postgres in the [runnable tutorial](tutorial/tutorial/main.hs).
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
