# database-generic

Database access without boilerplate, via `Generics`.

At the heart of database-generic is a typeclass called `MonadDB` which contains
a number of functions for communicating with a database. Here are some examples:

``` hs
select :: (MonadDB m, PrimaryKey a b) => b -> m (Maybe a)
insert :: (MonadDB m, Storable   a b) => a -> m ()
```

In order to send your data types through the `MonadDB` functions your data type
needs to A. derive `Generic` and B. implement `PrimaryKey`. Here is an example:

``` hs
data Employee = Employee { foo :: Int, bar :: String} deriving Generic

instance PrimaryKey Employee Int where
  primaryKey = .foo
```
