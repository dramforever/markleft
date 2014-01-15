markleft
========

Markleft marco engine in Haskell

Example Usage
-------------

```haskell
import Markleft

main = do let Right d = makeDocs "#concat{exam}{ple}"
              output = renderDocs std_table d
          
          putStrLn output
```

Documentation
-------------

No docs for now since:

- The source code is pretty clear. 
- The `std_table` is obsolete. More marcos coming
