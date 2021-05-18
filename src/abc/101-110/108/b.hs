main :: IO ()
main = do
  [x1,y1,x2,y2] <- map read . words <$> getLine
  let x1' = x1 - x2
      y1' = y1 - y2
      (x3', y3') = (y1', -x1')
      (x3, y3) = (x3' + x2, y3' + y2)
      x2' = x2 - x1
      y2' = y2 - y1
      (x4', y4') = (-y2', x2')
      (x4, y4) = (x4' + x1, y4' + y1)
  putStrLn $ unwords $ map show [x3, y3, x4, y4]
