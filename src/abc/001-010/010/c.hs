import Control.Monad

main :: IO ()
main = do
  [txa, tya, txb, tyb, t, v] <- map read . words <$> getLine
  n <- readLn
  xys <- replicateM n $ do
    [x, y] <- map read . words <$> getLine
    return (x, y)
  putStrLn $ if solve txa tya txb tyb t v xys then "YES" else "NO"

solve :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Bool
solve txa tya txb tyb t v = any f
  where
    f (x, y) = d1 + d2 <= fromIntegral (t * v)
      where
        d1 = sqrt $ fromIntegral ((txa - x) ^ 2 + (tya - y) ^ 2)
        d2 = sqrt $ fromIntegral ((txb - x) ^ 2 + (tyb - y) ^ 2)