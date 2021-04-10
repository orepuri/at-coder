import Control.Monad

main :: IO ()
main = do
  [w, h, n] <- map read . words <$> getLine
  ops <- replicateM n $ do
    [x, y, a] <- map read . words <$> getLine
    return (x, y, a)
  print $ solve w h ops

solve :: Int -> Int -> [(Int, Int, Int)] -> Int
solve w h ops = let ((w1,h1),(w2,h2)) = go ops ((0,0),(w,h))
                in (w2-w1) * (h2-h1)
  where
    go [] pos = pos
    go (op:ops) pos = go ops (next op pos)
    next (x,y,a) ((w1,h1),(w2,h2))
      | a == 1 = ((max x w1,h1),(max x w2,h2))
      | a == 2 = ((min x w1,h1),(min x w2,h2))
      | a == 3 = ((w1,max y h1),(w2,max y h2))
      | a == 4 = ((w1,min y h1),(w2,min y h2))
