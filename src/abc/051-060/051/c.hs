import Control.Monad

main :: IO ()
main = do
  [sx, sy, tx, ty] <- map read . words <$> getLine
  solve (tx - sx) (ty - sy)
  putStrLn ""

solve :: Int -> Int -> IO ()
solve x y = do
  replicateM_ y $ putChar 'U'
  replicateM_ x $ putChar 'R'
  replicateM_ y $ putChar 'D'
  replicateM_ x $ putChar 'L'
  putChar 'L'
  replicateM_ (y + 1) $ putChar 'U'
  replicateM_ (x + 1) $ putChar 'R'
  putChar 'D'
  putChar 'R'
  replicateM_ (y + 1) $ putChar 'D'
  replicateM_ (x + 1) $ putChar 'L'
  putChar 'U'

