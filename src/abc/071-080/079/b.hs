main :: IO ()
main = do
  n <- readLn
  print $ lucus n

lucus :: Int -> Int
lucus n
  | n == 0 = 2
  | n == 1 = 1
  | otherwise = lucusList !! (n - 1) + lucusList !! (n - 2)

lucusList :: [Int]
lucusList = map lucus [0..]