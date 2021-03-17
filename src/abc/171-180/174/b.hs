import Control.Monad

main :: IO ()
main = do
  [n, d] <- map read . words <$> getLine
  xys <- replicateM n $ do
    [x, y] <- map read . words <$> getLine
    return (x, y)
  print $ length $ filter (\(x, y) -> sqrt (x^2 + y^2) <= fromIntegral d ) xys 