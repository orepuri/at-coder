import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, d, h] <- map read . words <$> getLine
  dhs <- replicateM n $ do
    [d, h] <- map read . words <$> getLine
    return (d, h)
  let a = minimum $ map (\(d', h') -> fromIntegral (h - h') / fromIntegral (d - d')) dhs
  print $ solve d h a

solve :: Int -> Int -> Double -> Double
solve d h a = if b < 0 then 0.0 else b
  where
    b = fromIntegral h - a * fromIntegral d