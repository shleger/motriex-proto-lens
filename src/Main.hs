{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import GHC.Int (Int8)
import GHC.TypeLits (Symbol, symbolVal)
import Lens.Micro
import Lens.Micro.Extras (view)
import Network.GRPC.Client (CompressMode (..), RawReply, StreamDone (..), Timeout (..), open, singleRequest, streamReply, streamRequest)
import Network.GRPC.Client.Helpers (GrpcClientConfig (_grpcClientConfigCompression), grpcClientConfigSimple, rawUnary, setupGrpcClient, _grpcClientConfigHeaders)
import qualified Network.GRPC.Client.Helpers as Helper
import Network.GRPC.HTTP2.Encoding as Encoding
import Network.GRPC.HTTP2.ProtoLens as PL
import Network.HTTP2.Client (PortNumber)
import qualified Network.HTTP2.Client as Client
import Options.Generic
import qualified Proto.Marketdata as P
import qualified Proto.Marketdata_Fields as PF

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
    & #figi .~ ["BBG0013HJJ31", "BBG0013HGFT4"]

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell!" <> "OverloadedStrings"
  putStrLn $ "Hello, Haskell!" ++ "OverloadedStrings"
  putStrLn $ "::" <> show quot22
  print fooVal
  print quot22
  print reqLastPrices2
  print $ quot22 ^. field @"units" -- with TypeApplications, DataKinds ext
  -- read token from local storage
  tintoken <- readToken

  let connConfig =
        ConnConfig
          { host = "invest-public-api.tinkoff.ru",
            port = 443,
            tls = True,
            tintoken = tintoken
          }

  tinClient <- initGrpcConn connConfig

  resp <- runGrpc (prices tinClient reqLastPrices2)
  print resp

  putStrLn "Fin execution"

-- TODO add Exception case
readToken :: IO String
readToken = readFile ".tintoken"

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
