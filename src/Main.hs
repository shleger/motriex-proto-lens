module Main where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Proto.Marketdata as P

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

--   putStrLn quot

-- quot :: P.Quotation
-- quot =
--   defMessage
--     & #units .~ 123
--     & #nano .~ 567
