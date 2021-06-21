import Control.Monad

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  s <- C.getLine
  let n = C.length s
      gather s = do
        forM_ [1..C.length rs - 1] $ \_ -> print' 0
        print' rc
        print' lc
        forM_ [1..C.length ls - 1] $ \_ -> print' 0
        if C.null remain' then pure () else gather remain'
        where
          (rs, remain) = C.span (=='R') s
          (ls, remain') = C.span (=='L') remain
          rc = 1 + (C.length rs - 1 ) `div` 2 + ceiling (fromIntegral (C.length ls - 1) / 2)
          lc = 1 + ceiling (fromIntegral (C.length rs - 1) / 2) + (C.length ls -1 ) `div` 2
  gather s

print' s = putStr $ show s ++ " "