import Control.Monad

import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  xys <- replicateM n $ do
    [a, b] <- map read . words <$> getLine
    return (a, b)
  print $ solve n xys

solve :: Int -> [(Int, Int)] -> Double
solve n xys = total / fromIntegral (length routes)    
  where
    routes = permutations xys
    total :: Double
    total = sum $ map len routes
    len :: [(Int, Int)] -> Double
    len route = sum $ zipWith dis route (tail route)
    dis :: (Int, Int) -> (Int, Int) -> Double
    dis (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
