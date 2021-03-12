import Control.Monad
import Data.Foldable

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [x, y, p, q] <- map read . words <$> getLine
    case solve x y p q of
      Just ans -> print ans
      Nothing -> putStrLn "infinity"

solve :: Int -> Int -> Int -> Int -> Maybe Int
solve x y p q = if null times then Nothing else Just $ minimum times
  where
    times = [x..x+y-1] >>= f
    f :: Int -> [Int]
    f t1 = concatMap (toList . g t1) [p..p+q-1]
    g t1 t2 =
      fst <$> chineseRem t1 ((x + y) * 2) t2 (p + q)


extGcd :: Int -> Int -> (Int, Int, Int)
extGcd a 0 = (a, 1, 0)
extGcd a b =
  let (g, a', b') = extGcd b $ a `mod` b
  in (g, b', a' - a `div` b * b')

chineseRem :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
chineseRem b1 m1 b2 m2 =
  if (b2 - b1) `mod` g /= 0
  then Nothing
  else
    let m = m1 * (m2 `div` g)
        t = (b2 - b1) `div` g * p `mod` (m2 `div` g)
    in Just (b1 + m1 * t, m)
  where
    (g, p, q) = extGcd m1 m2