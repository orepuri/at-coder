import Control.Monad

main :: IO ()
main = do
  ss <- replicateM 12 getLine
  print $ length $ filter ('r' `elem`) ss 