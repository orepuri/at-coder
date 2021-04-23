import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- sort . map read . words <$> getLine
  print $ fst . foldl' (\(acc, pa) a -> (acc + a - pa, a)) (0, head as) $ tail as