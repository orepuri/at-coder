main :: IO ()
main = do
  x <- readLn :: IO Int
  let (a, b) = solve x
  putStrLn $ unwords $ map show [a, b]

solve :: Int -> (Int, Int)
solve x = head [(a, b) | a <- [-200..200], b <- [-200..200], a^5-b^5 == x]
