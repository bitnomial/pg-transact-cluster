# pg-transact-cluster

This library provides some abstractions for developing against a PostgreSQL cluster which has both read and write replicas. Developers can enclose `pg-transact` `DBT` values in a new type, marking them as either `ReadOnly` or `ReadWrite`. We provide a type class, so that code which consumes the queries does not need to care about the tagging as long as the type level tag is definite.

## Example usage

```haskell
main :: IO ()
main = do
    pool <- newClusterConnPool
                [connectPostgreSQL _readConnStringA , connectPostgreSQL _readConnStringB]
                [connectPostgreSOL _writeConn]
    resultR <- runSerializable readTask
    resultRW <- runSerializable writeTask

readTask :: CDB 'ReadOnly [Text]
readTask = getItems >>= followUpR

writeTask :: CDB 'ReadWrite ()
writeTask = asReadWrite getItems >>= followUpRW

getItems :: CDB anyMode [Int]
getItems = readonly q
  where
  q :: DB [Int]
  q = _impl

followUpR :: [Int] -> CDB anyMode [Text]
followUpR = readonly . q
  where
  q :: [Int] -> DB [Text]
  q = _impl

followUpW :: [Int] -> CDB 'ReadWrite ()
followUpW = readWrite . q
  where
    q :: [Int] -> DB ()
    q = _impl
```
