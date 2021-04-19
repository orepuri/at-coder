import Control.Monad
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n getLine
  putStrLn $ solve n ss

solve :: Int -> [String] -> String
solve n ss = concatMap (\c -> replicate (f c) c) ['a'..'z']
  where
    f c = minimum $ map (length . filter (==c)) ss

-- a - z まで, 各文字列での出現頻度をカウント
-- a - z までの出現頻度の少ない数の合計