
This is a tool for trying to detect whether or not a Haskell file has more than 1 unqualified entire import. 

It currently has issues with some files, look for "Parsing of file failed". 

Here are some test cases, there should be only one "bad" import per module.

```
import Data.List -- bad 

import Data.Monoid (Any(..)) -- good 

import Data.Maybe()  -- good 

import Data.Map as M -- bad 

import Data.Set qualified as S -- good 

import qualified Data.Function -- good 

import qualified Data.Bifunctor(bimap) -- good 

import Control.Monad.Freer (Eff, Member, interpret, reinterpret, type (~>)) -- good 
```

There still needs to be work done on detecting extensions. Likely, the path forward to this will be writing a quick check (either regex or parsec) on the file, and then doing a recursive upward search for cabal files. 