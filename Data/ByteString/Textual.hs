{-# LANGUAGE DeriveDataTypeable #-}

module Data.ByteString.Textual
    ( -- * ByteString tagged with encodings.
      TextualBytes(..)
    , toTextLenient
    , toTextStrict
    , toText
    , toBuilder
      -- * Type aliases
    , ASCIIBytes
    , UTF8Bytes
    , UTF16LEBytes
    , UTF16BEBytes
    , UTF32LEBytes
    , UTF32BEBytes
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
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Internal.Lazy.Encoding.Fusion as L
import Data.Text (Text)
import Data.Text.Internal.Encoding.Fusion
import Data.Text.Encoding.Error
import qualified Data.Text.Encoding as T
import Data.Text.Internal.Fusion.Types (Stream)
import qualified Data.Text.Internal.Fusion as F
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Exception (try, evaluate)

newtype TextualBytes a = TextualBytes { getBytes :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

toTextLenient :: Encoding a => TextualBytes a -> Text
toTextLenient tbs = toText lenientDecode tbs
{-# INLINE toTextLenient #-}

toTextStrict :: Encoding a => TextualBytes a -> Either UnicodeException Text
toTextStrict tbs = unsafeDupablePerformIO . try . evaluate $ toText strictDecode tbs
{-# INLINE toTextStrict #-}

toText :: Encoding a => OnDecodeError -> TextualBytes a -> Text
toText onErr tbs = F.unstream (streamDecode tbs onErr (getBytes tbs))
{-# INLINE [1] toText #-}

-- Use fast c utf8 decoder if possible
{-# RULES "toText/UTF8" [2] toText = toTextUTF8 #-}
toTextUTF8 :: OnDecodeError -> TextualBytes UTF8 -> Text
toTextUTF8 onErr (TextualBytes bs) = T.decodeUtf8With onErr bs

toBuilder :: TextualBytes a -> Builder
toBuilder (TextualBytes bs) = byteString bs
{-# INLINE toBuilder #-}

--------------------------------------------------------------------------------

class Encoding a where
    streamDecode :: p a -> OnDecodeError -> ByteString -> Stream Char
    streamDecodeLazy :: p a -> OnDecodeError -> L.ByteString -> Stream Char

data ASCII
data UTF8
data UTF16LE
data UTF16BE
data UTF32LE
data UTF32BE

type ASCIIBytes   = TextualBytes ASCII
type UTF8Bytes    = TextualBytes UTF8
type UTF16LEBytes = TextualBytes UTF16LE
type UTF16BEBytes = TextualBytes UTF16BE
type UTF32LEBytes = TextualBytes UTF32LE
type UTF32BEBytes = TextualBytes UTF32BE

instance Encoding ASCII   where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf8
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf8
instance Encoding UTF8    where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf8
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf8
instance Encoding UTF16LE where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf16LE
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf16LE
instance Encoding UTF16BE where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf16BE
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf16BE
instance Encoding UTF32LE where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf32LE
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf32LE
instance Encoding UTF32BE where
    {-# INLINE streamDecode #-}
    streamDecode _ = streamUtf32BE
    {-# INLINE streamDecodeLazy #-}
    streamDecodeLazy _ = L.streamUtf32BE
