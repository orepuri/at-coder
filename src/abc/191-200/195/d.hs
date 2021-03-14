import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m, q] <- map read . words <$> getLine
  wvs <- replicateM n $ do
    [w, v] <- map read . words <$> getLine
    return (v, w)
  let wvs' = sortBy (flip compare) wvs
  xs <- zip [1..] . map read . words <$> getLine
  replicateM_ q $ do
    [l, r] <- map read . words <$> getLine
    let xs' = sort . map snd $ filter (\(i, _) -> i < l || r < i) xs
    print $ solve wvs' xs' 0
  where
    solve _ [] acc = acc
    solve [] _ acc = acc
    solve ((v, w):vws) xs acc = case span (<w) xs of
      (_, []) -> solve vws xs acc
      (x1, _ : x2) -> solve vws (x1 ++ x2) (acc + v)
