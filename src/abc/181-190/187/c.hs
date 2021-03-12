import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List
import qualified Data.Set as S
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  (ss,ss') <- partition ((== '!') . C.head) <$> replicateM n C.getLine
  putStrLn $ solve ss ss'

solve :: [C.ByteString] -> [C.ByteString] -> String
solve ss ss' = maybe "satisfiable" C.unpack (find isUnsatisfiable ss')
  where
    set = S.fromList $ map C.tail ss
    isUnsatisfiable s = S.member s set
