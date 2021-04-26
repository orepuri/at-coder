import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  s <- VU.fromList . C.unpack <$> C.getLine
  q <- readLn :: IO Int
  qs <- VU.forM (VU.fromList [1..q]) $ \_ -> do
    [t, a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (t, a, b)
  s' <- VU.thaw s
  ans <- solve n (VUM.splitAt n s') qs
  putStrLn ans

type MVecChar = VUM.MVector (PrimState IO) Char

solve :: Int -> (MVecChar, MVecChar) -> VU.Vector (Int, Int, Int) -> IO String
solve n (s1, s2) qs = do
  (s1', s2') <- go (s1, s2) qs
  s1'' <- VU.freeze s1'
  s2'' <- VU.freeze s2'
  return $ VU.toList s1'' ++ VU.toList s2''
  where
    go :: (MVecChar, MVecChar) -> VU.Vector (Int, Int, Int) -> IO (MVecChar, MVecChar)
    go (s1, s2) qs
      | VU.null qs = do
          return (s1, s2)
      | otherwise = do
          if t == 1 then do
            let (a', b') = (min a b, max a b)
            if b' <= n then do
              VUM.swap s1 (a' - 1) (b' - 1)
            else if a > n then do
              VUM.swap s2 (a' - 1 - n) (b' - 1 - n)
            else do
              av <- VUM.read s1 (a' - 1)
              bv <- VUM.read s2 (b' - 1 - n)
              VUM.write s2 (b' - 1 - n) av
              VUM.write s1 (a' - 1) bv
            go (s1, s2) (VU.tail qs)
          else
            go (s2, s1) (VU.tail qs)
      where
        (t, a, b) = VU.head qs


-- FLIP
-- [IP] [FL]
-- 1 4
-- [LP] [FI]