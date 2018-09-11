
## `TypeRep`

```ghci
>>> :set -XTypeApplications
>>> import Data.Typeable
>>> typeRep (Proxy @(Either String Int))
Either [Char] Int
```

