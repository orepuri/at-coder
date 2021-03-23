import Data.List

main :: IO ()
main = do
  ns <- map read . words <$> getLine
  print $ solve ns

solve :: [Int] -> Int
solve ns = nub (sortBy (flip compare) [sum [a,b,c]|a<-ns,b<-ns,a/=b, c<-ns,b/=c,c/=a]) !! 2