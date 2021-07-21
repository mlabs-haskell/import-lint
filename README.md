
This is a tool for trying to detect whether or not a Haskell file has more than 1 unqualified entire import. 

It currently has great difficulty with ImportQualifiedPost, I tried writing a shotgun parser, it's not good, it errors on a lot of things, but I tried to give enough debug info to build a better one. 

Here are some test cases, there should be only one "bad" import per module.  

import Data.List -- bad 

import Data.Monoid (Any(..)) -- good 

import Data.Maybe()  -- good 

import Data.Map as M -- bad 

import Data.Set qualified as S -- good 

import qualified Data.Function -- good 

import qualified Data.Bifunctor(bimap) -- good 

import Control.Monad.Freer (Eff, Member, interpret, reinterpret, type (~>)) -- good 