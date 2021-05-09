import Data.List
import Data.Maybe
main :: IO ()
main = do
  n <- readLn
  as <- map read . words <$> getLine
  case solve n as of
    Just (bs, cs) -> do
      putStrLn "Yes"
      putStrLn $ unwords $ map show (length bs : map (\b -> 1 + fromJust (elemIndex b as)) bs)
      putStrLn $ unwords $ map show (length cs : map (\c -> 1 + fromJust (elemIndex c as)) cs)
    Nothing ->
      putStrLn "No"

solve :: Int -> [Int] -> Maybe ([Int], [Int])
solve n as = case cands of
    Just c ->
      let d = ans (head c) in Just (head d, d !! 1)
    Nothing -> Nothing
  where
    as' = tail $ take 201 $ subsequences as
    cands = find (\s -> length s > 1) 
      $ group
      $ sort 
      $ map (\s -> sum s `mod` 200) as'
    ans i = filter (\s -> sum s `mod` 200 == i) as'
