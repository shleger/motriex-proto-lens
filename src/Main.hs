module Main where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Proto.MarketData

main :: IO ()
main = putStrLn "Hello, Haskell!"
