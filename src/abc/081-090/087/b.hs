import Control.Monad

main :: IO ()
main = do
  [a, b, c, x] <- replicateM 4 readLn
  print $ solve a b c x

solve :: Int -> Int -> Int -> Int -> Int
solve a b c x = length $ [1 | ac <- [0..a], bc <- [0..b], cc <- [0..c], 500 * ac + 100 * bc + 50 * cc == x]

  