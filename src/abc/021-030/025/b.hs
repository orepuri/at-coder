import Control.Monad
import Text.Printf

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  ds <- replicateM n $ do
    [s, d] <- words <$> getLine
    if s == "East"
    then return $ read d 
    else return (-1 * read d)
  let ans = solve 0 a b ds
  if ans == 0 then print 0
  else printf "%s %d\n" (if ans < 0 then "West" else "East") $ abs ans

solve :: Int -> Int -> Int -> [Int] -> Int
solve c a b [] = c
solve c a b (d:ds) = solve c' a b ds
  where
    d' = abs d
    m = if d' < a then a else if b < d' then b else d'
    c' = c + if d < 0 then (-m) else m

