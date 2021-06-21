{-# Language BangPatterns #-}

import Control.Monad ()

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  [r, s, p] <- map read . words <$> getLine
  t <- C.getLine
  print $ solve n k r s p t

solve :: Int -> Int -> Int -> Int -> Int -> C.ByteString -> Int
solve n k r s p t = sum $ map go [0..k-1]
  where
    go !m = let (rs, ss, ps) = go2 (0, 0, 0) m in maximum [rs, ss ,ps]
    go2 (!rs, !ss, !ps) !i
      | i >= n = (rs, ss, ps)
      | otherwise = go2 (rs', ss', ps') (i + k)
      where
        pcHand = t `C.index` i
        (!rs', !ss', !ps') = case pcHand of
          'r' -> (max ss ps, p + max rs ps, max rs ss)
          's' -> (r + max ss ps, max rs ps, max rs ss)
          'p' -> (max ss ps, max rs ps, s + max rs ss)
          _ -> undefined 
