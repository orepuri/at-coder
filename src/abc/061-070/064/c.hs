import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  let (r1, r2) = solve as
  putStrLn $ unwords $ map show [r1, r2]

solve :: [Int] -> (Int, Int)
solve as = (max 1 colors, colors + frees)
  where
    colors = length . filter (/=9) $ M.keys freqs
    frees = fromMaybe 0 $ M.lookup 9 freqs
    freqs = foldl' (\acc a -> M.insertWith (+) (rate a) 1 acc) M.empty as
    rate a
      | a < 400 = 1
      | a < 800 = 2
      | a < 1200 = 3
      | a < 1600 = 4
      | a < 2000 = 5
      | a < 2400 = 6
      | a < 2800 = 7
      | a < 3200 = 8
      | otherwise = 9

