import qualified Data.ByteString.Char8 as C
import Data.Char

main :: IO ()
main = do
  [a,b] <- map C.unpack . C.words <$> C.getLine
  print $ solve a b

solve :: String -> String -> Int
solve a b = if sumA >= sumB then sumA else sumB
  where
    sumA = sum' a
    sumB = sum' b

sum' :: String -> Int
sum' s = sum $ map (\x -> ord x - ord '0') s
