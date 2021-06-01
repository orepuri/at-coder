import Control.Monad
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [gh, gw] <- map read . words <$> getLine
  grid <- AU.listArray ((1,1),(gh,gw)) . concat <$> replicateM gh getLine
  if gw == 1 && gh == 1 then
    putStrLn "No"
  else
    putStrLn $ if isSatisfied gw gh grid then "Yes" else "No"

isSatisfied :: Int -> Int -> AU.Array (Int, Int) Char -> Bool
isSatisfied gh gw grid = all p [(w,h)|w<-[1..gw],h<-[1..gh]]
  where
    p (w, h)
      | m == '.' = True
      | otherwise = any
         (\(w', h') -> grid AU.! (w', h') == '#')
          $ filter (\(w',h') -> w' >= 1 && w' <= gw && h' >= 1 && h' <= gh) [(w-1,h),(w+1,h),(w,h+1),(w,h-1)]
      where
        m = grid AU.! (w, h)
