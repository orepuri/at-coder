import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn
  print $ VU.length $ VU.filter (odd . length . show) $ VU.enumFromN (1::Int) n
  