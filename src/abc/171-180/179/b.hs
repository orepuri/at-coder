import Control.Monad

main :: IO ()
main = do
  n <- readLn
  ds <- replicateM n $ do
    [d1, d2] <- map read . words <$> getLine
    pure (d1, d2)
  putStrLn $ if solve ds then "Yes" else "No"

solve :: [(Int, Int)] -> Bool
solve ((d1, d2) : (e1, e2) : (f1, f2) : ds) =
  (d1 == d2 && e1 == e2 && f1 == f2)
  || 
  solve ((e1, e2) : (f1, f2) : ds)