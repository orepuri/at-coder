import Data.List
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  putStrLn $ unwords $ map show $ solve s

solve :: String -> [Int]
solve s = map f "ABCDEF"
  where
    f :: Char -> Int
    f c = fromMaybe 0 $ M.lookup c fq
    fq = M.fromList $ map (\s -> (head s, length s)) $ group $ sort s

  