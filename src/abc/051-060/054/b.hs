import Control.Monad

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  as <- replicateM n getLine
  bs <- replicateM m getLine
  putStrLn $ if solve n m as bs then "Yes" else "No"

solve :: Int -> Int -> [String] -> [String] -> Bool
solve n m as bs = or [ go (aw, ah) | aw <- [0..n-m], ah <- [0..n-m] ]
  where
    go :: (Int, Int) -> Bool
    go (aw, ah) =
      all (\(bw, bh) -> (bs !! bh) !! bw == (as !! (ah + bh)) !! (aw+bw)) [(bw, bh) | bw <- [0..m-1], bh <- [0..m-1]]

