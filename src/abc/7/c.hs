import Control.Monad
import qualified Data.Array.IO as AI
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Data.Maybe ( fromJust )

main :: IO ()
main = do
  [r,c] <- readIntList
  [sy,sx] <- readIntList
  [gy,gx] <- readIntList
  stage <- V.fromList <$> replicateM r C.getLine
  dp <- AI.newArray ((1,1), (r,c)) (-1)
  AI.writeArray dp (sy,sx) 0
  solve r c gy gx [(sy,sx)] stage dp
  print =<< AI.readArray dp (gy,gx)

solve :: Int -> Int -> Int -> Int -> [(Int,Int)] -> V.Vector C.ByteString -> AI.IOUArray (Int,Int) Int -> IO ()
solve r c gy gx [] stage dp = pure ()
solve r c gy gx (p:ps) stage dp = do
  nexts <- filterM (\(y,x) -> do
    d <- AI.readArray dp (y,x)
    pure $ (d == -1) && C.index (stage V.! (y-1)) (x-1) == '.') $ neighbors p
  d <- AI.readArray dp p
  forM_ nexts $ \n -> do
    AI.writeArray dp n (d+1)
  solve r c gy gx (ps++nexts) stage dp

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (r,c) = [(r-1,c),(r,c-1),(r+1,c),(r,c+1)]

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt