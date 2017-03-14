{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.TextualBytes.Encoding where

import GHC.Prim
import GHC.Types
import Data.ByteString.Builder.Prim

-- | A type class for texual decode/encode.
--
class TextualEncoding a where
    -- | peek a 'Char#' from given 'Addr#' and index, return both the 'Char#'
    -- and the advance offset in bytes on successful decoding, (# '0'#, 0# #) otherwise.
    --
    decodeChar :: p a
               -> Addr#  -- current chunk address
               -> Int#   -- current chunk length
               -> Addr#  -- next chunk address
               -> Int#   -- decoding index
               -> (# Char#, Int# #)

    -- | poke 'Char' using 'BoundedPrim'.
    --
    -- NOTE: Some encodings(ASCII for example) can't encode full range of a 'Char',
    -- truncate if that happens.
    --
    encodeChar :: p a -> BoundedPrim Char

between :: Word#               -- ^ byte to check
        -> Word#               -- ^ lower bound
        -> Word#               -- ^ upper bound
        -> Bool
between x y z = isTrue# (x `geWord#` y) && isTrue# (x `leWord#` z)
{-# INLINE between #-}

data ASCII
instance TextualEncoding ASCII where
    {-# INLINE decodeChar #-}
    decodeChar _ addr len next idx = (# indexCharOffAddr# addr idx, 1# #)

--------------------------------------------------------------------------------

data UTF8
instance TextualEncoding UTF8 where
    {-# INLINEABLE decodeChar #-}
    decodeChar _ addr len next idx =
        -- bound check
        case idx <# len of
            0# -> (# '\0'#, -1# #)
            1# ->
                let !x1  = indexWord8OffAddr# addr idx
                    !(Table utf8HeadTable#) = utf8HeadTable
                    !c = indexInt8OffAddr# utf8HeadTable# (word2Int# x1)
                in case c of
                    0xFF# -> (# '\0'#, 0# #)
                    _ ->
                        -- next bound check
                        let !cnt = case (c +# idx) <# len of
                                1# -> c
                                0# -> case next `eqAddr#` nullAddr# of
                                    0# -> 0xFF#
                                    1# -> len -# idx -# c
                        in case cnt of
                            0# -> (# chr# (word2Int# x1), 1# #)
                            1# ->
                                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                                in if utf8_validate2 x1 x2
                                    then (# utf8_chr2 x1 x2, 2# #)
                                    else (# '\0'#, 0# #)
                            2# ->
                                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                                in if utf8_validate3 x1 x2 x3
                                    then (# utf8_chr3 x1 x2 x3, 3# #)
                                    else (# '\0'#, 0# #)
                            3# ->
                                let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                                    !x3 = indexWord8OffAddr# addr (idx +# 2#)
                                    !x4 = indexWord8OffAddr# addr (idx +# 3#)
                                in if utf8_validate4 x1 x2 x3 x4
                                    then (# utf8_chr4 x1 x2 x3 x4, 4# #)
                                    else (# '\0'#, 0# #)
                            0# ->
                                let !x2 = indexWord8OffAddr# next 0#
                                in if utf8_validate2 x1 x2
                                    then (# utf8_chr2 x1 x2, 2# #)
                                    else (# '\0'#, 0# #)
                            -1# ->
                                let !x2 = indexWord8OffAddr# next 1#
                                    !x3 = indexWord8OffAddr# next 2#
                                in if utf8_validate3 x1 x2 x3
                                    then (# utf8_chr3 x1 x2 x3, 3# #)
                                    else (# '\0'#, 0# #)
                            -2# ->
                                let !x2 = indexWord8OffAddr# next 0#
                                    !x3 = indexWord8OffAddr# next 1#
                                    !x4 = indexWord8OffAddr# next 2#
                                in if utf8_validate4 x1 x2 x3 x4
                                    then (# utf8_chr4 x1 x2 x3 x4, 4# #)
                                    else (# '\0'#, 0# #)

                            0xFF# -> (# '\0'#, -2# #)

    {-# INLINEABLE encodeChar #-}
    encodeChar _ = charUtf8

{-# INLINE utf8_validate2 #-}
utf8_validate2 x1 x2 = (between x1 0xC2## 0xDF##) && (between x2 0x80## 0xBF##)

{-# INLINE utf8_validate3 #-}
utf8_validate3 x1 x2 x3 =
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

{-# INLINE utf8_validate4 #-}
utf8_validate4 x1 x2 x3 x4 =
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

{-# INLINE utf8_chr2 #-}
utf8_chr2 x1 x2 = chr# (word2Int# (
    or# (uncheckedShiftL# (and# x1 0x1f##) 6#)
        (and# x2 0x3f##)
    ))

{-# INLINE utf8_chr3 #-}
utf8_chr3 x1 x2 x3 = chr# (word2Int# (
    or# (uncheckedShiftL# (and# x1 0xf##) 12#)
        (or# (uncheckedShiftL# (and# x2 0x3f##) 6#)
             (and# x3 0x3f##))
    ))
{-# INLINE utf8_chr4 #-}
utf8_chr4 x1 x2 x3 x4 = chr# (word2Int# (
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
