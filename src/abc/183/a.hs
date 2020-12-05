{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  x <- readInt <$> C.getLine
  putStrLn $ show $ if x >= 0 then x else 0

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . C.readInt

