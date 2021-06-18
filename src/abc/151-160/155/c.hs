import Control.Monad

import qualified Data.ByteString.Char8 as C

import qualified Data.Map.Strict as M

import qualified Data.Vector as V

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- V.replicateM n C.getLine
  forM_ (solve n ss) $ \(s, _) -> do
    putStrLn $ C.unpack s

solve :: Int -> V.Vector C.ByteString -> [(C.ByteString, Int)]
solve n ss = M.toAscList $ M.filter (==count) freqs
  where
    freqs = V.foldl' (\acc s -> M.insertWith (+) s 1 acc) M.empty ss
    count = M.foldl' max 0 freqs