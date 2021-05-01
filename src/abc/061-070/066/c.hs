import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Char
import Data.List
import Control.Monad
import Control.Monad.Primitive

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VUM.replicate n 0 :: IO (VUM.MVector (PrimState IO) Int)
  forM_ (zip [n-1,n-2..] $ take n $ concatMap (\(a, b) -> [a-1,b-1]) $ zip [1..] [n,n-1..]) $ \(ai, bi) -> do
    VUM.write bs bi $ as VU.! ai
  bs' <- VU.freeze bs
  putStrLn $ unwords $ map show $ VU.toList bs'
