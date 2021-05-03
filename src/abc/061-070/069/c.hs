import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine 
  putStrLn $ if solve n as then "Yes" else "No"

solve :: Int -> VU.Vector Int -> Bool
solve n as = (mod4 + mod2 `div` 2) >= (n `div` 2)
  where
    (mod2, mod4) = VU.foldl' (\(mod2, mod4) a -> if a `mod` 4 == 0 then (mod2, mod4 + 1) else if even a then (mod2 + 1, mod4) else (mod2, mod4)) (0, 0) as

-- a1 * a2 が4の倍数
-- * いずれかが4の倍数
-- * 両方とも2の倍数
-- n `div` 2 個が条件を満たせばOK?