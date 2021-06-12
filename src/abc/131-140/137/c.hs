import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- V.replicateM n $ C.sort <$> C.getLine
  print $ solve n ss

solve :: Int -> V.Vector C.ByteString -> Int
solve n as = sum $ map (\(_, f) -> sum [1..f-1]) $ M.toList freqs
  where
    freqs = V.foldl' (\acc s -> M.insertWith (+) s 1 acc) M.empty as
