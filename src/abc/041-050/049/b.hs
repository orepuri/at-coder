import Control.Monad

main :: IO ()
main = do
  [ph, pw] <- map read . words <$> getLine
  forM_ [1..ph] $ \h -> do
    cs <- getLine
    putStrLn cs
    putStrLn cs