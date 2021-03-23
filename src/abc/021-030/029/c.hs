import Control.Monad

main :: IO ()
main = do
  n <- readLn
  forM_ (replicateM n "abc") putStrLn
