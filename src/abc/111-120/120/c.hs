import Control.Monad
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  s <- C.getLine
  print $ solve s

solve :: C.ByteString -> Int
solve s = C.length s - length (go [] s)
  where
    go stack s
      | C.null s = stack
      | null stack = go [sh] st
      | sh /= sh' = go st' st
      | otherwise = go (sh:stack) st
      where
        sh = C.head s
        st = C.tail s
        sh' = head stack
        st' = tail stack
