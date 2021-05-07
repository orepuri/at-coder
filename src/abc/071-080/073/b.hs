import Control.Monad

main :: IO ()
main = do
  n <- readLn
  lrs <- replicateM n $ do
    [l, r] <- map read . words <$> getLine
    return $ r - l + 1
  print $ sum lrs