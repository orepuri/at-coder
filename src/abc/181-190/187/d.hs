import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as Intro

main :: IO ()
main = do
  n <- readLn :: IO Int 
  votes <- replicateM n $ do
    [a,b] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    pure (a,b)
  let sorted = sortBy (flip compare `on` (\(a,b) -> 2*a+b)) votes
  print $ solve sorted n

solve :: [(Int, Int)] -> Int -> Int
solve votes allCities = allCities - length (solve' votes aokiVotes 0)
  where
    aokiVotes = foldl (\acc (a,_) -> acc + a) 0 votes

solve' :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
solve' (v:vs) aokiVotes takahashiVotes =
  let aokiVotes' = aokiVotes - fst v
      takahashiVotes' = takahashiVotes + uncurry (+) v
  in if takahashiVotes' > aokiVotes' then vs
  else solve' vs aokiVotes' takahashiVotes'