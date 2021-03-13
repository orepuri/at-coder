import Control.Monad

main :: IO ()
main = do
  n <- readLn
  ng1 <- readLn
  ng2 <- readLn
  ng3 <- readLn
  putStrLn $ if solve n [ng1, ng2, ng3] then "YES" else "NO"

solve :: Int -> [Int] -> Bool
solve n ngs = n `notElem` ngs && loop 0 n
  where
    loop :: Int -> Int -> Bool
    loop 100 acc = acc <= 0
    loop i acc
      | n' `elem` ngs = if n'' `elem` ngs
                      then
                        if n''' `elem` ngs
                        then False
                        else loop (i+1) n'''
                      else loop (i+1) n''
      | otherwise = loop (i+1) n'
      where
        n' = acc - 3
        n'' = acc - 2
        n''' = acc - 1
      
-- n ==100