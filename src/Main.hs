{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- maybe needed

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

-}

module Main where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Proto.Marketdata as P

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell!" <> "OverloadedStrings"
  putStrLn $ "Hello, Haskell!" ++ "OverloadedStrings"
  putStrLn $ "::" <> show quot22
  print fooVal
  print quot22

fooVal :: P.Quotation
fooVal = defMessage & #units .~ 42

quot22 :: P.Quotation
quot22 =
  defMessage
    & #units .~ 123
    & #nano .~ 567
