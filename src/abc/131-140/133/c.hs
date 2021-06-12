import Control.Monad

import Data.List

main :: IO ()
main = do
  [l, r] <- map read . words <$> getLine
  print $ solve l r

solve :: Int -> Int -> Int
solve l r
  | r - l >= 2019 = 0
  | otherwise = minimum [x*y`mod`2019|x<-[l..r-1],y<-[x+1..r]]
