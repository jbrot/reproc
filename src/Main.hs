{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Monad.ST
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Bool
import Data.Char
import Data.Csv
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

type CsvRow = Map B.ByteString B.ByteString

median :: (Ord a, Fractional a) => [a] -> a
median l' 
  | len `mod` 2 == 1 = l !! (len `div` 2)
  | otherwise             = (/2) . sum . take 2 . drop ((len `div` 2) - 1) $ l
  where l = sort l'
        len = length l

pfrac :: (Read a, Fractional a) => B.ByteString -> a
pfrac = read . fmap repl . T.unpack . T.decodeUtf8
    where repl ',' = '.'
          repl x   = x

processColumn :: B.ByteString -> B.ByteString -> Vector CsvRow -> Vector CsvRow
processColumn fcol col csv = fmap (adjust medians) csv
    where medians :: M.Map B.ByteString Double
          medians = fmap (median . fmap pfrac) split
          split = foldr buildSplit M.empty csv
          buildSplit :: CsvRow -> M.Map B.ByteString [B.ByteString] -> M.Map B.ByteString [B.ByteString]
          buildSplit row map = maybe map (\cval -> M.alter (\x -> Just $ (fold x <>) . maybe [] pure . M.lookup col $ row)
                                                            cval map)
                                         (M.lookup fcol row)
          adjust :: M.Map B.ByteString Double -> CsvRow -> CsvRow
          adjust meds row = maybe row (\cval -> M.adjust (B.pack . show . (\x -> x - cval) . pfrac) col row) $
              flip M.lookup meds =<< M.lookup fcol row
          

process :: B.ByteString -> [B.ByteString] -> Vector CsvRow -> Vector CsvRow
process key cols = foldr (.) id (processColumn key <$> cols)

main :: IO ()
main = do
    let dopts = defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') }
        eopts = defaultEncodeOptions { encDelimiter = fromIntegral (ord '|') }
        key = "UNITID"
        vals = [ "SRS", "ADM_RATE", "ADM_RATE_ALL", "SAT_AVG", "SAT_AVG_ALL", "SATVR25", "SATMT25"
               , "SATMT75" ]
    raw <- BL.readFile "sat2.csv"
    let vec :: Vector Int
        vec = V.fromList [1..10]
        vec2 = sort (pure vec)
    (h, b) <- either fail pure $ decodeByNameWith dopts raw
    BL.writeFile "sat2p.csv" . encodeByNameWith eopts h . V.toList . process key vals $ b
    return ()
