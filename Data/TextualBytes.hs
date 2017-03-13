{-# LANGUAGE DeriveDataTypeable #-}

module Data.ByteString.Textual
    ( -- * ByteString tagged with encodings.
      TextualBytes(..)
    , toTextLenient
    , toTextStrict
    , toText
    , toBuilder
      -- * Encodings
    , Encoding(..)
    , ASCII
    , UTF8
    , UTF16LE
    , UTF16BE
    , UTF32LE
    , UTF32BE
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Builder (Builder, byteString)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Exception (try, evaluate)
import GHC.Prim
import GHC.Types
import GHC.ForeignPtr (ForeignPtr(..))

import Data.TextualBytes.Encoding

newtype TextualBytes a = TextualBytes { getBytes :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

decodeLenient :: Encoding a => TextualBytes a -> String
toTextLenient = decodeInternal False

decodeThrow :: Encoding a => TextualBytes a -> String
decodeThrow = decodeInternal True

decodeInternal :: Encoding a => Bool -> TextualBytes a -> (# String, Int# )
decodeInternal thw tbs = go 0# []
  where
    PS (ForeignPtr addr _) (I# off) (I# len) = getBytes tbs
    dec = decode tbs (addr `plusAddr#` off)
    go i =
        let (# chr, l #) = dec i
            i' = i +# l#
        in case i' <# len of
            1# -> case l of
                0# -> if thw then error "!!"
                             else '\xfffd' : go i'
                _ -> C# chr : go i'
            0# ->


