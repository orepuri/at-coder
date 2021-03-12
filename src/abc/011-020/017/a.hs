import Control.Monad

main :: IO ()
main = do
  scores <- forM [0..2] $ \_ -> do
    [s, e] <- map read . words <$> getLine :: IO [Int]
    return $ s * e
  print $ sum scores `div` 10