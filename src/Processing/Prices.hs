{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}



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
import Data.ProtoLens.Message (defMessage)
import Data.ProtoLens.Labels ()
import Data.Int (Int64)
import Proto.Marketdata (LastPrice)
import Proto.Marketdata_Fields (figi)
import Data.Text (Text) 
import Data.Text as DT (unpack)
import Proto.Google.Protobuf.Timestamp (Timestamp)

import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time (UTCTime)
import qualified Proto.Google.Protobuf.Duration_Fields as PF
import Data.Time.Clock (NominalDiffTime)
import Data.Data (Typeable)

import qualified Data.ByteString as T




prices :: Helper.GrpcClient -> P.GetLastPricesRequest -> Client.ClientIO (Either Client.TooMuchConcurrency (RawReply P.GetLastPricesResponse))
prices = rawUnary (PL.RPC :: PL.RPC P.MarketDataService "getLastPrices" )


data CurSymTin = BBG0013HJJ31 | BBG0013HGFT4 deriving (Enum, Typeable, Read)

instance Show CurSymTin where
  show :: CurSymTin -> String
  show BBG0013HJJ31 = "EURRUB"
  show BBG0013HGFT4 = "USDRUB"



reqLastPrices2 :: P.GetLastPricesRequest
reqLastPrices2 =
  defMessage
    & #figi .~ ["BBG0013HJJ31", "BBG0013HGFT4"] --EUR,USD

data Currency = Currency { symbol :: !CurSymTin, value :: !String , time :: !UTCTime} deriving Show



toCurrency :: LastPrice -> Currency
toCurrency lp = do

    let p1p2 = show p1 ++ "." ++ show p2 where
            p1  = lp ^. PF.price . PF.units
            p2  = fromIntegral $ lp ^. PF.price . PF.nano

    Currency
        { symbol = read $ DT.unpack $ lp ^. PF.figi,
          value = p1p2,
          time = posixSecondsToUTCTime $ fromIntegral $ lp ^. PF.time . PF.seconds
        }



lastPricesFetch::  Helper.GrpcClient -> IO()
lastPricesFetch tinClient = do
    resp <- (^. PF.lastPrices) <$> runGrpc (prices tinClient reqLastPrices2)

    let curs = map toCurrency resp

    -- TODO  wrap to json
    print (show curs)
