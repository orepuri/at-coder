import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve (VU.tail as) (VU.head as) 1

solve :: VU.Vector Int -> Int -> Int -> Int
solve as pa acc
  | VU.null as = sum [1..acc]
  | a > pa = solve as' a (acc + 1)
  | otherwise = sum [1..acc] + solve as' a 1
  where
    a = VU.head as
    as' = VU.tail as


-- acc = 条件を満たす数
-- 条件を満たさなくなったらそれまでの総件数を計算. 条件を満たす数をクリアして継続
--  例: i = 1,2,3 まで条件を満たしていた場合, 1は3, 2は2, 3は1つあるので sum [1..3]が総数
