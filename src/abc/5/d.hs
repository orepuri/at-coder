import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readInt
  cs <- fmap (V.scanl (VU.zipWith (+)) (VU.replicate (n + 1) 0)) $ V.replicateM n $ do
    VU.fromList . scanl (+) 0 <$> readIntList
  let ans = VU.create $ do
              ds <- VUM.replicate (n ^ 2 + 1) 0
              forM_ [1..n] $ \i -> do
                forM_ [1..n] $ \j -> do
                  forM_ [i..n] $ \k -> do
                    forM_ [j..n] $ \l -> do
                      let numberOfTacoyaki = (k - i + 1) * (l - j + 1)
                      let s1 = cs V.! k VU.! l
                      let s2 = cs V.! (i - 1) VU.! (j - 1)
                      let s3 = cs V.! k VU.! (j - 1)
                      let s4 = cs V.! (i - 1) VU.! l
                      VUM.modify ds (max (s1 + s2 - s3 - s4)) numberOfTacoyaki
              return ds
  let ans' = VU.scanl1 max ans  -- たこ焼きをより多く焼ける少ないマス数を反映
  q <- readInt
  replicateM_ q $ do
    p <- readInt
    print $ ans' VU.! p

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

readInt :: IO Int
readInt = toInt <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt