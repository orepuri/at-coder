import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  putStrLn $ if innerProduct as bs == 0 then "Yes" else "No"

innerProduct :: VU.Vector Int -> VU.Vector Int -> Int
innerProduct as bs = VU.sum $ VU.map (uncurry (*)) $ VU.zip as bs
