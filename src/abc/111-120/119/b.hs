import Control.Monad

main :: IO ()
main = do
  n <- readLn
  xus <- replicateM n $ do
    [x, u] <- words <$> getLine
    return (read x, u)
  print $ solve xus

solve :: [(Double, String)] -> Double
solve xus = sum $ map (\(x, u) -> if u == "JPY" then x else x * 380000.0) xus
