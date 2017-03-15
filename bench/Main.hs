module Main where

import           Criterion.Main
import qualified Data.ByteString as B
import qualified Data.Text.Internal.Encoding.Fusion as TF
import qualified Data.Text.Internal.Fusion.Common as TF
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.TextualBytes as TB
import qualified Data.TextualBytes.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Char (toUpper)

main :: IO ()
main = do
    bs <- B.readFile "./test/utf8-sample.txt"
    defaultMain
        [ bgroup "decode utf-8"
            [ bench "text" $ nf (T.unpack . T.decodeUtf8) bs
            , bench "text-packed" $ nf (T.decodeUtf8) bs
            , bench "text-stream" $ nf (TF.unstreamList . TF.streamUtf8 TE.lenientDecode) bs
            , bench "texualBytes" $ nf (TB.decodeLenient) (TB.TextualBytes bs :: TB.TextualBytes TB.UTF8)
            , bench "texualBytes-packed" $ nf
                ((TB.encode :: String -> TB.TextualBytes TB.UTF8 )
                .(TB.decodeLenient :: TB.TextualBytes TB.UTF8 -> String))
                (TB.TextualBytes bs)

            ]
        , bgroup "map utf-8"
            [ bench "text" $ nf (T.length . T.map toUpper . T.decodeUtf8) bs
            , bench "texualBytes" $ nf (sum . map fromEnum . TB.decodeLenient) (TB.TextualBytes bs :: TB.TextualBytes TB.UTF8)
            ]

        , bgroup "length uft-8"
            [ bench "text" $ nf (T.length . T.decodeUtf8) bs
            , bench "texualBytes" $ nf (TB.validate) (TB.TextualBytes bs :: TB.TextualBytes TB.UTF8)
            ]
        ]
