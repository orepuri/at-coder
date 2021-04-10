import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  ts <- map read . words <$> getLine
  let total = sum ts
  m <- readLn
  forM_ [1..m] $ \_ -> do
    [p, x] <- map read . words <$> getLine
    let pt = ts !! (p - 1)
    print $ total - pt + x
