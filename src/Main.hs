{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON, genericToEncoding, defaultOptions )
import Data.Aeson.Types (toEncoding)
import Network.GRPC.ClientInit (newConnection)
import Processing.Prices (lastPricesFetch)

-- client




-- TODO add Exception case
readToken :: IO String
readToken = readFile "/home/saa/.config/i3/py3status/.tintoken"

main :: IO ()
main =
  do
    tintoken <- readToken
    tinClient <- newConnection tintoken
    lastPricesFetch tinClient

data Person = Person
  { name :: Text,
    age :: Int
  }
  deriving (Generic, Show)

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

