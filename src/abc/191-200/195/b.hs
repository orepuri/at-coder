import Text.Printf

main :: IO ()
main = do
  [a, b, w] <- map read . words <$> getLine :: IO [Double]
  let w' = w * 1000
      a' = floor (w' / a) :: Int
      b' = ceiling (w' / b) :: Int
  if b' > a'
    then putStrLn "UNSATISFIABLE"
    else printf "%d %d\n" b' a'


--- 100-200 2000(2)

--- 120 150 2000(2)

---- 120 => 16.6 => 16
---- 150 => 13.3 => 14

---- 300 => 3.03 => 3
---- 333 => 3.003 => 4