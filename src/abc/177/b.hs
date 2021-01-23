import Data.List

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  print $ solve s t

solve :: String -> String -> Int
solve s t = minimum . map (diff t) . take (l - m + 1) . tails $ s
  where
    l = length s
    m = length t

diff s' t' = length $ filter (==True) $ zipWith (/=) s' t'