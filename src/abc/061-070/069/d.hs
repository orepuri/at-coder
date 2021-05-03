import Data.List

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  let grids = concatMap (uncurry replicate) $ zip as [1..]
      go :: [Int] -> Int -> IO ()
      go grids i
        | null grids = pure ()
        | even i = do
            let (row, remains) = splitAt w grids
            putStrLn $ unwords $ map show row
            go remains (i + 1)
        | otherwise = do
            let (row, remains) = splitAt w grids
            putStrLn $ unwords $ map show $ reverse row
            go remains (i + 1)
  go grids 0
  