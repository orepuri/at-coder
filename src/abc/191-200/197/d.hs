import Text.Printf

main :: IO ()
main = do
  n <- readLn
  [x0,y0] <- map read . words <$> getLine
  [xn2,yn2] <- map read . words <$> getLine
  let (x1, y1) = solve n x0 y0 xn2 yn2
  printf "%f %f" x1 y1

solve :: Int -> Int -> Int -> Int -> Int -> (Double, Double)
solve n x0 y0 xn2 yn2 = (x1, y1)
  where
    cx = fromIntegral (xn2 + x0) / 2
    cy = fromIntegral (yn2 + y0) / 2
    x0' = fromIntegral x0 - cx
    y0' = fromIntegral y0 - cy
    a = 2 * pi / fromIntegral n
    x1' = x0' * cos a - y0' * sin a
    y1' = y0' * cos a + x0' * sin a
    x1 = x1' + cx
    y1 = y1' + cy
