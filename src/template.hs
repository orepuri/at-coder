{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
mport Data.Maybe

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  putStrLn $ show $ solve

solve :: Int
solve = undefined

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . C.readInt

readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . C.words
