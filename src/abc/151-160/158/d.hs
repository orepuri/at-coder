{-# LANGUAGE BangPatterns #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

data QUERY = Reverse | Cons !Char | Snoc !Char
type Flipped = Bool

main :: IO ()
main = do
  s <- getLine
  gq <- readLn
  qs <- V.replicateM gq $ do
    q <- C.getLine
    return $ case C.index q 0 of
      '1' -> Reverse
      '2' -> case C.index q 2 of
        '1' -> Cons (C.last q)
        '2' -> Snoc (C.last q)
        _ -> undefined 
      _ -> undefined 
  putStrLn $ solve s qs

solve :: String -> V.Vector QUERY -> String
solve s qs = go (s, "") qs False
  where
    go (s1, s2) qs flipped
      | V.null qs = if flipped then s2 ++ reverse s1 else s1 ++ reverse s2
      | otherwise = case q of
          Reverse -> go (s1, s2) qs' (not flipped)
          Cons c -> if flipped then go (s1, c:s2) qs' flipped else go (c:s1, s2) qs' flipped
          Snoc c -> if flipped then go (c:s1, s2) qs' flipped else go (s1, c:s2) qs' flipped
      where 
        q = V.head qs
        qs' = V.tail qs
