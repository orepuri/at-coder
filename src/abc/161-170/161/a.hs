import Control.Monad

main :: IO ()
main = do
  [x, y, z] <- map read . words <$> getLine :: IO [Int]
  forM_ [z,x,y] print
