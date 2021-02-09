-- WA
carry = 10 ^ 4 :: Int
 
main :: IO ()
main = do
  [cx, cy, r] <- map (round . (* fromIntegral carry) . read) . words <$> getLine
  let xu = carry * floor (fromIntegral (cx + r) / fromIntegral carry)
      xl = carry * ceiling (fromIntegral (cx - r) / fromIntegral carry)
      yu = carry * floor (fromIntegral (cy + r) / fromIntegral carry)
      yl = carry * ceiling (fromIntegral (cy - r) / fromIntegral carry)
      r2 = r * r
  let f1 y' = ceiling $ (fromIntegral cx - sqrt (fromIntegral (r2 - (cy - y')^2))) / fromIntegral carry
      f2 y' = floor $ (fromIntegral cx + sqrt (fromIntegral (r2 - (cy - y')^2))) / fromIntegral carry
  print $ sum $ map (\y' -> f2 y' - f1 y' + 1) [yl, yl + carry.. yu]