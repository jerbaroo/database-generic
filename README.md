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

## Roadmap

| Feature                                                          | Status |
|------------------------------------------------------------------|--------|
| Derive Entity for simple data types                              | ✅     |
| Write Entity to PostgreSQL via MonadDb                           | ✅     |
| Read Entity from PostgreSQL via MonadDb                          | ✅     |
| Delete Entity from PostgreSQL via MonadDb                        | ✅     |
| Execute stream of statements against PostgreSQL via Conduit      |        |
| Listen for and stream Entity updates from PostgreSQL via Conduit |        |
| Servant server: endpoint to execute single statement             |        |
| Servant server: execute stream of statements over WebSocket      |        |
| Servant server: stream Entity updates from server over WebSocket |        |
| Servant server permission checks                                 |        |
| Reflex (client-side) MonadDb instance                            |        |

## Quick Start

A tutorial as code exists [here](example/Main.hs).

To run the tutorial on your machine:
1. Clone this repo.
2. Start a PostgreSQL instance with username and password `demo`, e.g.:
  `docker run -it --rm --env POSTGRES_PASSWORD=demo --env POSTGRES_USER=demo --publish 5432:5432 postgres`
3. Either `cabal run` via provided `nix-shell`, or `stack run`.
