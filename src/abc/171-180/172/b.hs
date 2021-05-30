import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  s <- C.getLine
  t <- C.getLine
  print $ solve s t

solve :: C.ByteString -> C.ByteString -> Int
solve s t = go s t 0
  where
    go s t acc
      | C.null s = acc
      | sh == th = go st tt acc
      | otherwise = go st tt (acc + 1)  
      where
        sh = C.head s
        th = C.head t
        st = C.tail s
        tt = C.tail t
        