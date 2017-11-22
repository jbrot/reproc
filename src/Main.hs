{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Bool
import Data.Char
import Data.Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

type CsvRow = Map B.ByteString B.ByteString

median :: Fractional a => Vector a -> a
median l 
    | V.length l `mod` 2 == 1 = l V.! (V.length l `div` 2)
    | otherwise             = (/2) . V.sum . V.slice ((V.length l `div` 2) - 1) 2 $ l

pfrac :: (Read a, Fractional a) => B.ByteString -> a
pfrac = read . T.unpack . T.decodeUtf8

processColumn :: B.ByteString -> B.ByteString -> Vector CsvRow -> Vector CsvRow
processColumn fcol col csv = fmap (adjust medians) csv
    where medians :: M.Map B.ByteString Double
          medians = fmap (median . fmap pfrac) split
          split = foldr buildSplit M.empty csv
          buildSplit :: CsvRow -> M.Map B.ByteString (Vector B.ByteString) -> M.Map B.ByteString (Vector B.ByteString)
          buildSplit row map = maybe map (\cval -> M.adjust ((<>) . maybe V.empty pure . M.lookup col $ row)
                                                            cval map)
                                         (M.lookup fcol row)
          adjust :: M.Map B.ByteString Double -> CsvRow -> CsvRow
          adjust meds row = maybe row (\cval -> M.adjust (B.pack . show . (\x -> x - cval) . pfrac) col row) $
              flip M.lookup meds =<< M.lookup fcol row
          

process :: B.ByteString -> [B.ByteString] -> (Header, Vector CsvRow) -> (Header, Vector CsvRow)
process key cols = second $ foldr (.) id (processColumn key <$> cols)

main :: IO ()
main = do
    let dopts = defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') }
        eopts = defaultEncodeOptions { encDelimiter = fromIntegral (ord '|') }
        key = "UNITID"
        vals = [ "SRS", "ADM_RATE", "ADM_RATE_ALL", "SAT_AVG", "SAT_AVG_ALL", "SATVR25", "SATMT25"
               , "SATMT75" ]
    raw <- BL.readFile "sat2.csv"
    (h, b) <- either fail (pure . process key vals) $ decodeByNameWith dopts raw
    BL.writeFile "sat2p.csv" . encodeByNameWith eopts h . V.toList $ b
    return ()
