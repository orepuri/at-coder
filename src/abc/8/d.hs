import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Data.Maybe

type Memo = M.Map (Point, Point) Int
type Point = (Int, Int)

main :: IO ()
main = do
  [w,h] <- readIntList
  n <- readLn :: IO Int
  locations <- map (\[x,y] -> (x, y)) <$> replicateM n readIntList
  let (ans, _) = dp locations M.empty ((1,1), (w,h))
  print ans

dp :: [Point] -> Memo -> (Point, Point) -> (Int, Memo)
dp locations memo (ul,lr) = case M.lookup (ul,lr) memo of
  Just v -> (v, memo)
  Nothing -> let memo' = foldl' (dp' locations (ul,lr)) memo locations
    in (fromJust $ M.lookup (ul,lr) memo', memo')

dp' :: [Point] -> (Point, Point) -> Memo -> Point -> Memo
dp' locations (ul,lr) memo location =
  if location `isIn` (ul,lr) then
    let (x,y) = location
        (l,t) = ul
        (r,b) = lr
        (a1, memo1) = dp locations memo  (ul,(x-1,y-1))
        (a2, memo2) = dp locations memo1 ((x+1,t),(r,y-1))
        (a3, memo3) = dp locations memo2 ((l,y+1),(x-1,b))
        (a4, memo4) = dp locations memo3 ((x+1,y+1),lr)
        golds = a1+a2+a3+a4+b-t+r-l+1
    in case M.lookup (ul,lr) memo of
      Just v ->
        M.insert (ul,lr) (max v golds) memo4
      Nothing ->
        M.insert (ul,lr) golds memo4
  else
    case M.lookup (ul,lr) memo of
      Just _ ->
        memo
      Nothing ->
        M.insert (ul,lr) 0 memo

isIn :: Point -> (Point, Point) -> Bool
isIn (x,y) ((x1,y1), (x2,y2)) =
  x1 <= x && x <= x2 && y1 <= y && y <= y2

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine 

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt