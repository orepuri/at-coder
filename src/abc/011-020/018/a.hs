import Data.List
import Data.Maybe

main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn
  c <- readLn
  let order = reverse . sort $ [a, b, c]
  print $ 1 + fromJust (elemIndex a order)
  print $ 1 + fromJust (elemIndex b order)
  print $ 1 + fromJust (elemIndex c order)