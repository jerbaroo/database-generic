# database-generic

Database agnostic typeclass to access generically persisted data.

## Introduction

Explanation of the above:
- Database agnostic typeclass: the typeclass is called `MonadDb` and you must
  specify how it can communicate with your database (e.g. PostgreSQL server)
- Generically persisted data: you can derive an instance of `Entity` for your
  data types via `Generics` (or write your own instance). `MonadDb` can then
  read/write instances of `Entity` to/from your database.

A key intended feature of this library is that the typeclass `MonadDb` can be
used either server-side or client-side. Allowing your client application (e.g.
web app) to use the same functions to access data as you use server-side.

Another important intended feature is an optional `servant` server. Merely
provide an instance of `MonadDb` so the server knows how to communicate with
your database, then the server can act as a proxy to allow clients to read/write
to your database without having to write the usual server boilerplate.

## Quick Start

A tutorial as code exists [here](tutorial/Main.hs).

To run the tutorial on your machine:
1. Clone this repo.
2. Start a PostgreSQL instance with username and password `demo`, e.g.:
  `docker run -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish 5432:5432 postgres`
3. Either `cabal run` via provided `nix-shell`, or `stack run`.

## Features

| Feature                                      | Status | Tested |
|----------------------------------------------|--------|--------|
| Create table                                 | ✅     | ✅     |
| Select all                                   | ✅     | ✅     |
| Select by ID                                 | ✅     | ✅     |
| Return subset of fields                      | ✅     | ✅     |
| Where column equals                          |        |        |
| Where column is null                         |        |        |
| Where column is not null                     |        |        |
| Order by clause                              | ✅     | ✅     |
| Order by asc/desc                            |        |        |
| Limit clause                                 | ✅     | ✅     |
| Offset clause                                | ✅     | ✅     |
| Insert one                                   | ✅     |        |
| Insert many                                  | ✅     |        |
| Insert returning                             | ✅     |        |
| Delete all                                   | ✅     | ✅     |
| Delete by ID                                 | ✅     | ✅     |
| Delete returning                             | ✅     | ✅     |
| Stream statements over Conduit               |        |        |
| Stream Entity updates over Conduit           |        |        |
| Server: endpoint to execute statement        |        |        |
| Server: stream statements over WebSocket     |        |        |
| Server: stream Entity updates over WebSocket |        |        |
| Server: permission checks                    |        |        |
| Reflex (client-side) MonadDb instance        |        |        |
| Joins                                        |        |        |
