{-# LANGUAGE BangPatterns #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAI

import Debug.Trace
import Text.Printf

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  pys <- VU.replicateM m $ do
    [p, y] <- map read . words <$> getLine
    return (p, y)
  pys' <- VU.thaw pys
  VAI.sortBy (\(_, y1) (_, y2) -> compare y1 y2) pys'
  m <- solve <$> VU.freeze pys'
  VU.forM_ pys $ \(p, y) -> do
    let order = M.findWithDefault 0 (p, y) m
    printf "%06d%06d\n" p order

solve :: VU.Vector (Int, Int) -> M.Map (Int,Int) Int
solve pys = fst $ VU.foldl' go (M.empty, M.empty) pys
  where
    go (!acc, !numbering) (!p, !y) = (M.insert (p, y) v acc, M.insert p v numbering)
      where
        !v = 1 + M.findWithDefault 0 p numbering
