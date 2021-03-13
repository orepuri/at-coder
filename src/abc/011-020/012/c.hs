import Control.Monad
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  forM_ (solve n) $ \(a, b) -> do
    printf "%d x %d\n" a b

solve :: Int -> [(Int, Int)]
solve n = [(a, b)| a <- [1..9], b <- [1..9], a * b == diff]
  where
    diff = 2025 - n
