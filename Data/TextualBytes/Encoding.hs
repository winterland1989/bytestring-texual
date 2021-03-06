{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.TextualBytes.Encoding where

import GHC.Prim
import GHC.Types
import Data.ByteString.Builder.Prim

class TextualEncoding a where
    maximumBytes :: p a -> Int#
    validateChar :: p a -> Addr# -> Int# -> Int#
    decodeChar :: p a -> Addr# -> Int# -> (# Char#, Int# #)
    decodeEdge :: p a -> Addr# -> Int# -> Addr# -> (# Char#, Int# #)
    encodeChar :: p a -> BoundedPrim Char

between :: Word#               -- ^ byte to check
        -> Word#               -- ^ lower bound
        -> Word#               -- ^ upper bound
        -> Bool
between x y z = isTrue# (x `geWord#` y) && isTrue# (x `leWord#` z)
{-# INLINE between #-}

data ASCII
instance TextualEncoding ASCII where
    {-# INLINEABLE validateChar #-}
    validateChar _ _ _ = 1#
    {-# INLINEABLE decodeChar #-}
    decodeChar _ addr idx = (# indexCharOffAddr# addr idx, 1# #)
    {-# INLINEABLE decodeEdge #-}
    decodeEdge _ addr idx _ = (# indexCharOffAddr# addr idx, 0# #)

--------------------------------------------------------------------------------

data UTF8
instance TextualEncoding UTF8 where
    {-# INLINEABLE validateChar #-}
    -- Reference: https://howardhinnant.github.io/utf_summary.html
    --
    validateChar _ addr idx =
        case cnt of
            0# -> 1#
            1# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                in if validate2 x1 x2
                    then 2#
                    else 0#
            2# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                in if validate3 x1 x2 x3
                    then 3#
                    else 0#
            3# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                    !x4 = indexWord8OffAddr# addr (idx +# 3#)
                in if validate4 x1 x2 x3 x4
                    then 4#
                    else 0#
            _  -> 0#

      where !x1  = indexWord8OffAddr# addr idx
            !(Table utf8HeadTable#) = utf8HeadTable
            !cnt = indexInt8OffAddr# utf8HeadTable# (word2Int# x1)

    {-# INLINEABLE decodeChar #-}
    decodeChar _ addr idx =
        case cnt of
            0# -> (# chr# (word2Int# x1), 1# #)
            1# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                in if validate2 x1 x2
                    then (# chr2 x1 x2, 2# #)
                    else (# '\0'#, 0# #)
            2# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                in if validate3 x1 x2 x3
                    then (# chr3 x1 x2 x3, 3# #)
                    else (# '\0'#, 0# #)
            3# ->
                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                    !x4 = indexWord8OffAddr# addr (idx +# 3#)
                in if validate4 x1 x2 x3 x4
                    then (# chr4 x1 x2 x3 x4, 4# #)
                    else (# '\0'#, 0# #)
            _  -> (# '\0'#, 0# #)

      where !x1  = indexWord8OffAddr# addr idx
            !(Table utf8HeadTable#) = utf8HeadTable
            !cnt = indexInt8OffAddr# utf8HeadTable# (word2Int# x1)

    {-# INLINE encodeChar #-}
    encodeChar _ = charUtf8

{-# INLINE validate2 #-}
validate2 x1 x2 = (between x1 0xC2## 0xDF##) && (between x2 0x80## 0xBF##)

{-# INLINE validate3 #-}
validate3 x1 x2 x3 =
    isTrue# (x1 `eqWord#` 0xE0##) &&
        between x2 0xA0## 0xBF## &&
        between x3 0x80## 0xBF##
    || between x1 0xE1## 0xEC## &&
        between x2 0x80## 0xBF## &&
        between x3 0x80## 0xBF##
    || isTrue# (x1 `eqWord#` 0xED##) &&
        between x2 0x80## 0x9F## &&
        between x3 0x80## 0xBF##
    || between x1 0xEE## 0xEF## &&
        between x2 0x80## 0xBF## &&
        between x3 0x80## 0xBF##

{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 =
    isTrue# (x1 `eqWord#` 0xF0##) &&
        between x2 0x90## 0xBF## &&
        between x3 0x80## 0xBF## &&
        between x4 0x80## 0xBF##
    || between x1 0xF1## 0xF3## &&
        between x2 0x80## 0xBF## &&
        between x3 0x80## 0xBF## &&
        between x4 0x80## 0xBF##
    || isTrue# (x1 `eqWord#` 0xF4##) &&
        between x2 0x80## 0x8F## &&
        between x3 0x80## 0xBF## &&
        between x4 0x80## 0xBF##

{-# INLINE chr2 #-}
chr2 x1 x2 = chr# (word2Int# (
    or# (uncheckedShiftL# (and# x1 0x1f##) 6#)
        (and# x2 0x3f##)
    ))

{-# INLINE chr3 #-}
chr3 x1 x2 x3 = chr# (word2Int# (
    or# (uncheckedShiftL# (and# x1 0xf##) 12#)
        (or# (uncheckedShiftL# (and# x2 0x3f##) 6#)
             (and# x3 0x3f##))
    ))
{-# INLINE chr4 #-}
chr4 x1 x2 x3 x4 = chr# (word2Int# (
    or# (uncheckedShiftL# (and# x1 0x7##) 18#)
        (or# (uncheckedShiftL# (and# x2 0x3f##) 12#)
            (or# (uncheckedShiftL# (and# x3 0x3f##) 6#)
                (and# x4 0x3f##)))
    ))

data Table = Table Addr#
utf8HeadTable :: Table
utf8HeadTable = Table
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\
    \\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\
    \\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\
    \\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff"#
{-# NOINLINE utf8HeadTable #-}

--------------------------------------------------------------------------------
