import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    [n,s,k] <- readIntList
    print $ solve n s k

solve :: Int -> Int -> Int -> Int
solve n s k = if s `mod` g /= 0 then -1
    else ((-s') * x) `mod` n'
  where
    (g, x, _) = extgcd k n
    n' = n `div` g
    s' = s `div` g


extgcd :: Int -> Int -> (Int, Int, Int)
extgcd a 0 = (a, 1, 0)
extgcd a b =
  let (g, x', y') = extgcd b $ a `mod` b
  in (g, y', x' - a `div` b * y')

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt