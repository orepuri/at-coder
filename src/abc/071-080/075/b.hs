import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char

main :: IO ()
main = do
  [gh, gw] <- map read . words <$> getLine
  gridList <- forM [1..gh] $ \h -> do
    s <- getLine
    forM [1..gw] $ \w -> do
      return ((w, h), s !! (w-1))
  let grid = array ((1, 1), (gw, gh)) $ concat gridList :: UArray (Int, Int) Char
  forM_ [1..gh] $ \h -> do
    forM_ [1..gw] $ \w -> do
      let mass = grid ! (w, h)
      if mass == '.'
        then
          putChar $ intToDigit $ bombs gw gh w h grid
        else
          putChar mass
    putChar '\n'

bombs :: Int -> Int -> Int -> Int -> UArray (Int, Int) Char -> Int
bombs gw gh w h grid = length $ filter (=='#') [grid ! (w', h') | (w', h') <- masses, w' > 0, h' > 0, w' <= gw, h' <= gh] 
  where
    masses = [
       (w-1, h-1), (w, h-1), (w+1, h-1)
      ,(w-1, h), (w, h), (w+1, h)
      ,(w-1, h+1), (w, h+1), (w+1, h+1)]
  