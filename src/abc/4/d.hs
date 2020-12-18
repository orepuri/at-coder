{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  [r,g,b] <- toIntList <$> BS.getLine
  putStrLn $ show $ solve r g b

solve :: Int -> Int -> Int -> Int
solve r g b =
  minimum $ map (solve' r g b) [(-350) .. 350]

solve' :: Int -> Int -> Int -> Int -> Int
solve' r g b gs = gc + rc + bc
  where
    gc = greenCount gs g
    rc = redCount r gs
    bc = blueCount b (gs + g - 1)

greenCount :: Int -> Int -> Int
greenCount gs g = sum $ map abs [gs..gs+g-1]

redCount :: Int -> Int -> Int
redCount r gs = sum $ map abs [relativeRs..relativeRs+r-1]
  where
    rHome = -100
    rHomeToGs = abs (rHome - gs)
    rs = if gs > rHome && r `div` 2 < rHomeToGs then rHome - r `div` 2
      else gs - r
    --  !x = trace ("rs = " ++ show rs) 1
    relativeRs = rs - rHome

blueCount :: Int -> Int -> Int
blueCount b ge = sum $ map abs [relativeBs..relativeBs+b-1]
  where
    bHome = 100
    bHomeToGe = abs (bHome - ge)
    bs = if ge < bHome && b `div` 2 < bHomeToGe then bHome - b `div` 2
      else ge + 1
    -- !x = trace ("rbs = " ++ show relativeBs) 1
    relativeBs = bs - bHome

toIntList :: C.ByteString -> [Int]
toIntList = map toInt . C.words

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt
