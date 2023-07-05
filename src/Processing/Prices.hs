{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}



module Processing.Prices where

import qualified Network.GRPC.Client.Helpers as Helper
import qualified "http2-client" Network.HTTP2.Client as Client

import Network.GRPC.HTTP2.ProtoLens as PL (RPC (..))


import qualified Proto.Marketdata as P
import qualified Proto.Marketdata as PF
import qualified Proto.Marketdata_Fields as PF
import Network.GRPC.ClientProto (RawReply)
import Network.GRPC.Client.Helpers (rawUnary)

import Lens.Micro ((&), (.~), (^.))
import Network.GRPC.ClientInit (runGrpc)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (field)
import Data.ProtoLens.Labels ()



prices :: Helper.GrpcClient -> P.GetLastPricesRequest -> Client.ClientIO (Either Client.TooMuchConcurrency (RawReply P.GetLastPricesResponse))
prices = rawUnary (PL.RPC :: PL.RPC P.MarketDataService "getLastPrices" )    

reqLastPrices2 :: P.GetLastPricesRequest
reqLastPrices2 =
  defMessage
    & #figi .~ ["BBG0013HJJ31", "BBG0013HGFT4"] --EUR,USD


lastPricesFetch::  Helper.GrpcClient -> IO()
lastPricesFetch tinClient = do
    resp <- (^. PF.lastPrices) <$> runGrpc (prices tinClient reqLastPrices2)

    let f = resp !! 1
    let p1 = f ^. PF.price . PF.units
    let p2 = fromIntegral $ f ^. PF.price . PF.nano
    let p1p2 = show p1 ++ "." ++ show p2

    print (show p1p2)
