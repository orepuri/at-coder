import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [n,m] <- readIntList
  putStrLn $ unwords . map show $ solve n m

solve :: Int -> Int -> [Int]
solve n m = if null candidates then [-1,-1,-1] else head candidates
  where candidates = [[adults, olds, babies] | olds <- [0..1],
                                               let adults = 2 * n - (m + olds) `div` 2,
                                               let babies = n - adults - olds,
                                               adults >= 0, babies >= 0, even (m + olds)]

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt