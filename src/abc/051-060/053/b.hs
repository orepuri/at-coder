import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  s <- C.getLine
  print $ solve s

solve :: C.ByteString -> Int
solve s = z - a + 1
  where
    a = fromJust $ C.elemIndex 'A' s
    z = fromJust $ C.elemIndexEnd 'Z' s
