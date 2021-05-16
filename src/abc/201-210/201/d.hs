import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Array.IO
import Data.Array.IArray
import Data.List
import qualified Data.Array.Unboxed as AU

main :: IO ()
main = do
  [gh, gw] <- map read . words <$> getLine
  gridList <- replicateM gh getLine
  let grid = AU.listArray ((1,1),(gw,gh)) $ concat $ transpose gridList :: AU.UArray (Int, Int) Char
  scores <- newArray ((1,1),(gw,gh)) minBound :: IO (IOUArray (Int,Int) Int)

  let go :: Int -> Int -> IO Int
      go w h = do
        rightScore <- do
          if w >= gw then return 0
          else do
            s <- readArray scores (w + 1, h)
            if s == minBound then go (w + 1) h else return s
        downScore <- do
          if h >= gh then return 0
          else do
            s <- readArray scores (w, h + 1)
            if s == minBound then go w (h + 1) else return s

        if even (w + h)
        then do
          let s | w + 1 > gw && h + 1 > gh = 0
                | w + 1 > gw = downScore + score w (h + 1)
                | h + 1 > gh = rightScore + score (w + 1) h
                | otherwise = max (rightScore + score (w + 1) h) (downScore + score w (h + 1))
          writeArray scores (w, h) s
          return s
        else do
          let s | w + 1 > gw && h + 1 > gh = 0
                | w + 1 > gw = downScore - score w (h + 1)
                | h + 1 > gh = rightScore - score (w + 1) h
                | otherwise = min (rightScore - score (w + 1) h) (downScore - score w (h + 1))
          writeArray scores (w, h) s
          return s
        where
          score i j
            | i > gw || j > gh = 0
            | grid ! (i, j) == '+' = 1
            | otherwise = -1
  
  go 1 1

  score <- readArray scores (1, 1)
  putStrLn $ if score > 0 then  "Takahashi" else if score < 0 then "Aoki" else  "Draw"
