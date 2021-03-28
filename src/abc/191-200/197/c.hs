import Data.Bits
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = minimum $ map f $ splitIntoSublists n as
  where
    f xss = foldl' xor 0 $ map (foldl' (.|.) 0) xss

splitIntoSublists :: Int -> [Int] -> [[[Int]]]
splitIntoSublists n xs = map (go xs) bs
  where
    bs :: [[Bool]]
    bs = mapM (const [True,False]) [1..n-1]
    go :: [Int] -> [Bool] -> [[Int]]
    go [x] [] = [[x]]
    go (x:xs) (True:bs) = [x] : go xs bs
    go (x:xs) (False:bs) = let r = go xs bs in (x : head r) : tail r
