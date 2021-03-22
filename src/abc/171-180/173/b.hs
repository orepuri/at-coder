import Control.Monad
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n getLine
  forM_ ["AC", "WA", "TLE", "RE"] $ \r -> do
    printf "%s x %d\n" r $ length $ filter (==r) ss