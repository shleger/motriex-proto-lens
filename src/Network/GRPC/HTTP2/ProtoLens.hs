{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Copied from http2-grpc-proto-len package because of unsupported cabal dependency proto-lens (>=0.5 && <0.7), in my project
-- | Source: https://github.com/haskell-grpc-native/http2-grpc-haskell/tree/master/http2-grpc-proto-lens/src/Network/GRPC/HTTP2
module Network.GRPC.HTTP2.ProtoLens (RPC (..)) where

import Data.Binary.Builder (fromByteString, putWord32be, singleton)
import Data.Binary.Get (getByteString, getInt8, getWord32be, runGetIncremental)
import qualified Data.ByteString.Char8 as ByteString
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (Message)
import Data.ProtoLens.Service.Types (HasMethod, HasMethodImpl (..), Service (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, symbolVal)

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif

import Network.GRPC.HTTP2.Encoding
import Network.GRPC.HTTP2.Types

-- | A proxy type for giving static information about RPCs.
data RPC (s :: *) (m :: Symbol) = RPC

instance (Service s, HasMethod s m) => IsRPC (RPC s m) where
  path rpc = "/" <> pkg rpc Proxy <> "." <> srv rpc Proxy <> "/" <> meth rpc Proxy
    where
      pkg :: (Service s) => RPC s m -> Proxy (ServicePackage s) -> HeaderValue
      pkg _ p = ByteString.pack $ symbolVal p

      srv :: (Service s) => RPC s m -> Proxy (ServiceName s) -> HeaderValue
      srv _ p = ByteString.pack $ symbolVal p

      meth :: (Service s, HasMethod s m) => RPC s m -> Proxy (MethodName s m) -> HeaderValue
      meth _ p = ByteString.pack $ symbolVal p
  {-# INLINE path #-}

instance (Service s, HasMethod s m, i ~ MethodInput s m) => GRPCInput (RPC s m) i where
  encodeInput _ = encode
  decodeInput _ = decoder

instance (Service s, HasMethod s m, i ~ MethodOutput s m) => GRPCOutput (RPC s m) i where
  encodeOutput _ = encode
  decodeOutput _ = decoder

-- | Decoder for gRPC/HTTP2-encoded Protobuf messages.
decoder :: Message a => Compression -> Decoder (Either String a)
decoder compression = runGetIncremental $ do
  isCompressed <- getInt8 -- 1byte
  let decompress = if isCompressed == 0 then pure else _decompressionFunction compression
  n <- getWord32be -- 4bytes
  decodeMessage <$> (decompress =<< getByteString (fromIntegral n))

-- | Encodes as binary using gRPC/HTTP2 framing.
encode :: Message m => Compression -> m -> Builder
encode compression plain =
  mconcat
    [ singleton (if _compressionByteSet compression then 1 else 0),
      putWord32be (fromIntegral $ ByteString.length bin),
      fromByteString bin
    ]
  where
    bin = _compressionFunction compression $ encodeMessage plain
