import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n C.getLine
  putStrLn $ C.unpack $ solve ss

solve :: [C.ByteString ] -> C.ByteString
solve ss = head . snd $ maximum zipped
  where
    grouped = group $ sort ss
    zipped = zip (map length grouped) grouped