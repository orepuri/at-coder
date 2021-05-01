import Data.List
import Data.Maybe

main :: IO ()
main = do
  xs <- map read . words <$> getLine
  print $ 1 + fromJust (elemIndex 0 xs)