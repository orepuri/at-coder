import Data.Char

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  let as = replicate b (intToDigit a)
      bs = replicate a (intToDigit b)
  putStrLn $ minimum [as, bs]