module Data.ByteString.Textual.Lazy where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Textual (Encoding(..), ASCII, UTF8, UTF16LE, UTF16BE, UTF32LE, UTF32BE)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Internal.Lazy.Encoding.Fusion
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy.Encoding as T
import Data.Text.Internal.Fusion.Types (Stream)
import qualified Data.Text.Internal.Lazy.Fusion as F
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Exception (try, evaluate)

newtype TextualBytes a = TextualBytes { getBytes :: ByteString }

type ASCIIBytes   = TextualBytes ASCII
type UTF8Bytes    = TextualBytes UTF8
type UTF16LEBytes = TextualBytes UTF16LE
type UTF16BEBytes = TextualBytes UTF16BE
type UTF32LEBytes = TextualBytes UTF32LE
type UTF32BEBytes = TextualBytes UTF32BE

toTextLenient :: Encoding a => TextualBytes a -> Text
toTextLenient tbs = toText lenientDecode tbs
{-# INLINE toTextLenient #-}

toTextStrict :: Encoding a => TextualBytes a -> Either UnicodeException Text
toTextStrict tbs = unsafeDupablePerformIO . try . evaluate $ toText strictDecode tbs
{-# INLINE toTextStrict #-}

toText :: Encoding a => OnDecodeError -> TextualBytes a -> Text
toText onErr tbs = F.unstream (streamDecodeLazy tbs onErr (getBytes tbs))
{-# INLINE [1] toText #-}

-- Use fast c utf8 decoder if possible
{-# RULES "toText/UTF8" [2] toText = toTextUTF8 #-}
toTextUTF8 :: OnDecodeError -> TextualBytes UTF8 -> Text
toTextUTF8 onErr (TextualBytes lbs) = T.decodeUtf8With onErr lbs

toBuilder :: TextualBytes a -> Builder
toBuilder (TextualBytes lbs) = lazyByteString lbs
{-# INLINE toBuilder #-}
