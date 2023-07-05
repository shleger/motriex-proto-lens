{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}

module Network.GRPC.ClientInit where


import qualified Network.GRPC.Client.Helpers as Helper
import qualified "http2-client" Network.HTTP2.Client as Client
import Network.GRPC.Client.Helpers (GrpcClientConfig (_grpcClientConfigCompression), grpcClientConfigSimple, rawUnary, setupGrpcClient, _grpcClientConfigHeaders)
import Network.GRPC.ClientProto (CompressMode (..), RawReply, StreamDone (..), Timeout (..), open, singleRequest, streamReply, streamRequest)

import Data.Text as T (pack)
import Data.Text 
import qualified Data.ByteString.Char8 as BC (pack)

import Control.Exception (Exception, throwIO)




data ConnConfig = ConnConfig {host :: String, port :: Client.PortNumber, tintoken :: String, tls :: Bool}

initGrpcConn :: ConnConfig -> IO Helper.GrpcClient
initGrpcConn igc =
  Client.runClientIO (setupGrpcClient . prepGrpcConn $ igc) >>= \case
    Right client -> pure client
    Left err -> throwIO . ClientSetupError . T.pack $ show err

prepGrpcConn :: ConnConfig -> GrpcClientConfig
prepGrpcConn conf = do
  let grpcConfig = grpcClientConfigSimple (host conf) (port conf) (tls conf)
  -- update record
  let bearer = BC.pack ("Bearer " ++ tintoken conf)
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


newConnection:: String  -> IO Helper.GrpcClient
newConnection tintoken = do
    let connConfig =
          ConnConfig
            { host = "invest-public-api.tinkoff.ru",
              port = 443,
              tls = True,
              tintoken = tintoken
            }
    initGrpcConn connConfig

    



