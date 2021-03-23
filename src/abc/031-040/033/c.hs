import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  s <- C.getLine
  print $ solve s

solve :: C.ByteString -> Int
solve s = length $ filter (notElem (C.pack "0") . C.split '*') terms
  where
    terms = C.split '+' s
