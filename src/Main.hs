{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE PackageImports               #-}


{- maybe needed

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

phase2:
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-}

module Main where

-- client

import Control.Concurrent.Async.Lifted (replicateConcurrently)
import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Aeson.TH (defaultOptions)
import Data.Aeson.Types (ToJSON (..), genericToEncoding)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Char8 as ByteString
import Data.Maybe (fromMaybe, maybeToList)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (field)
import Data.ProtoLens.Labels ()
import Data.String (fromString)
import Data.Text as T (pack)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Int (Int8)
import GHC.TypeLits (Symbol, symbolVal)
import Lens.Micro ( (&), (.~), (^.) )
import Lens.Micro.Extras (view)
import Network.GRPC.ClientMy (CompressMode (..), RawReply, StreamDone (..), Timeout (..), open, singleRequest, streamReply, streamRequest)
import Network.GRPC.Client.Helpers (GrpcClientConfig (_grpcClientConfigCompression), grpcClientConfigSimple, rawUnary, setupGrpcClient, _grpcClientConfigHeaders)
import qualified Network.GRPC.Client.Helpers as Helper
import Network.GRPC.HTTP2.Encoding as Encoding ()
import Network.GRPC.HTTP2.ProtoLens as PL ( RPC(..) )
import "http2-client" Network.HTTP2.Client (PortNumber)
import qualified "http2-client" Network.HTTP2.Client as Client
import Options.Generic ( Generic, Text )
import qualified Proto.Marketdata as P
import qualified Proto.Marketdata_Fields as PF
import Text.Printf (printf)
import Text.Read ( readMaybe )
import qualified Proto.Marketdata as PF
-- import Piesync.Data.ProtoLen



fooVal :: P.Quotation
fooVal = defMessage & #units .~ 42

quot22 :: P.Quotation
quot22 =
  defMessage
    & #units .~ 123
    & #nano .~ 567

reqLastPrices2 :: P.GetLastPricesRequest
reqLastPrices2 =
  defMessage
    & #figi .~ ["BBG0013HJJ31", "BBG0013HGFT4"] --EUR,USD

data CurrencyLastPrice = CurrencyLastPrice
  { code :: String,
    value :: Float
  }
  deriving (Generic, Show)

instance ToJSON CurrencyLastPrice

curEUR = CurrencyLastPrice "ASD" 123

main :: IO ()
main =
  do
    -- print (encode curEUR)

    {-
    putStrLn $ "Hello, Haskell!" <> "OverloadedStrings"
    putStrLn $ "Hello, Haskell!" ++ "OverloadedStrings"
    putStrLn $ "::" <> show quot22
    print fooVal
    print quot22
    print reqLastPrices2
    print $ quot22 ^. field @"units" -- with TypeApplications, DataKinds ext
    -- read token from local storage
    -}
    tintoken <- readToken

    let connConfig =
          ConnConfig
            { host = "invest-public-api.tinkoff.ru",
              port = 443,
              tls = True,
              tintoken = tintoken
            }

    tinClient <- initGrpcConn connConfig

    resp <- (^. PF.lastPrices) <$> runGrpc (prices tinClient reqLastPrices2)

    let f =  resp !! 1
    let p1 = f ^. PF.price . PF.units
    let p2 = fromIntegral $ f ^. PF.price . PF.nano
    let p1p2 = show p1 ++ "." ++ show p2
    -- let json = encodeMessageJSON f
    let pp = Person{name="dd", age=123}
    let ppJson = encodePretty pp

    print  (show p1p2)

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show) 

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions 


-- TODO add Exception case
readToken :: IO String
readToken = readFile ".tintoken"

output :: Maybe Float -> Float
output (Just x) = x
output Nothing = 0.0

prices :: Helper.GrpcClient -> P.GetLastPricesRequest -> Client.ClientIO (Either Client.TooMuchConcurrency (RawReply P.GetLastPricesResponse))
prices = rawUnary (PL.RPC :: PL.RPC P.MarketDataService "getLastPrices")

data ConnConfig = ConnConfig {host :: String, port :: PortNumber, tintoken :: String, tls :: Bool}

initGrpcConn :: ConnConfig -> IO Helper.GrpcClient
initGrpcConn igc =
  Client.runClientIO (setupGrpcClient . prepGrpcConn $ igc) >>= \case
    Right client -> pure client
    Left err -> throwIO . ClientSetupError . T.pack $ show err

prepGrpcConn :: ConnConfig -> GrpcClientConfig
prepGrpcConn conf = do
  let grpcConfig = grpcClientConfigSimple (host conf) (port conf) (tls conf)
  -- update record
  let bearer = BC.pack ("Bearer " ++ (tintoken conf))
  grpcConfig {_grpcClientConfigHeaders = [("Authorization", bearer)]}

----- =====

data SDKError = ClientSetupError Text | GrpcError Text
  deriving (Show)

instance Exception SDKError

runGrpc :: Client.ClientIO (Either Client.TooMuchConcurrency (RawReply a)) -> IO a
runGrpc f =
  Client.runClientIO f >>= \case
    Right (Right (Right (_, _, Right res))) -> pure res
    Right (Right (Right (_, _, Left err))) -> error . show $ GrpcError (T.pack $ show err)
    Right (Right (Left err)) -> error . show $ GrpcError (T.pack $ show err)
    Right (Left err) -> error . show $ GrpcError (T.pack $ show err)
    Left err -> error . show $ GrpcError (T.pack $ show err)
