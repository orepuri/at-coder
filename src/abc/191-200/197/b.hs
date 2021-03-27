import Control.Monad

main :: IO ()
main = do
  [h, w, x, y] <- map read . words <$> getLine :: IO [Int]
  ss <- replicateM h getLine
  print $ solve ss x y

solve :: [String] -> Int -> Int -> Int
solve ss x y = xu + xd + yu + yd - 1
  where
    a = reverse $ take x ss
    b = map (\a -> a !! (y-1)) a 
    xu = length $ takeWhile (=='.') b
    xd = length $ takeWhile (=='.') $ map (\s -> s !! (y-1)) $ drop x ss
    row = ss !! (x-1)
    yu = length $ takeWhile (=='.') $ reverse $ take y row
    yd = length $ takeWhile (=='.') $ drop y row

    
