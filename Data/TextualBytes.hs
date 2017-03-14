{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.TextualBytes
    ( -- * ByteString tagged with encodings.
      TextualBytes(..)
    , decodeThrow
    , decodeLenient
    , validate
    , encode
      -- * Encodings
    , TextualEncoding(..)
    , ASCII
    , UTF8
    ) where

import Data.Data
import Control.DeepSeq
import GHC.Generics
import Data.Typeable
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import Data.ByteString.Builder.Prim
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Builder (Builder, byteString)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Exception (try, evaluate)
import GHC.Exts (build)
import GHC.Prim
import GHC.Types
import GHC.ForeignPtr (ForeignPtr(..))
import Data.Proxy

import Data.TextualBytes.Encoding

newtype TextualBytes a = TextualBytes { getBytes :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NFData (TextualBytes a)

validate :: TextualEncoding a => TextualBytes a -> Int
validate tbs = I# (go 0# 0#)
  where
    !(PS (ForeignPtr addr _) (I# off) (I# len)) = getBytes tbs
    dec = validateChar tbs (addr `plusAddr#` off)
    go i acc =
        let l = dec i
            i' = i +# l
        in case i' <# len of
            1# -> case l of
                0# -> 0#
                _ -> go i' (1# +# acc)
            0# -> acc
{-# INLINE validate #-}

decodeLenient :: TextualEncoding a => TextualBytes a -> String
decodeLenient = decodeInternal False

decodeThrow :: TextualEncoding a => TextualBytes a -> String
decodeThrow = decodeInternal True

encode :: forall e. TextualEncoding e => String -> TextualBytes e
encode cs =
    let lbs = toLazyByteString $ primMapListBounded (encodeChar (Proxy :: Proxy e)) cs
    in TextualBytes (toStrict lbs)

--------------------------------------------------------------------------------

decodeInternal :: TextualEncoding a => Bool -> TextualBytes a -> String
decodeInternal thw tbs = go 0#
  where
    !(PS (ForeignPtr addr _) (I# off) (I# len)) = getBytes tbs
    dec = decodeChar tbs (addr `plusAddr#` off)
    go i =
        let (# chr, l #) = dec i
            i' = i +# l
        in case i' <# len of
            1# -> case l of
                0# -> if thw then error $
                                "Data.TextualBytes.decodeInternal: can't decode byte at " ++ show (I# i)
                             else '\xfffd' : go i'
                _ -> C# chr : go i'
            0# -> [C# chr]
{-# INLINE decodeInternal #-}
