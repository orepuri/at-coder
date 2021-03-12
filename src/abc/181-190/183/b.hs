{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [sx,sy,gx,gy] <- readIntList <$> C.getLine
  putStrLn $ show $ solve sx sy gx gy


solve :: Int -> Int -> Int -> Int -> Double
solve sx sy gx gy = sx' + sy' * ((gx' - sx') / (gy' + sy'))
  where
    [sx',sy',gx',gy'] = map fromIntegral [sx,sy,gx,gy]

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . C.readInt

readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . C.words
