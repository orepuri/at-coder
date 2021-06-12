import Control.Monad
import Text.Printf

main :: IO ()
main = do
  [w, h, x, y] <- map read . words <$> getLine
  let area = fromIntegral (w * h) / 2 :: Double
  let count = if 2 * x == w && 2 * y == h then 1 else 0 :: Int
  printf "%f %d\n" area count
