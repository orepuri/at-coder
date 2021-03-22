import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  rs <- replicateM n $ do
    read <$> getLine
  print $ solve rs

solve :: [Int] -> Double
solve rs = (*) pi $ fst . foldr f (0.0, 1) $ sort rs
  where
    f r (ans, sig) = (ans + sq r * sig, sig * (-1))
    sq r = fromIntegral $ r^2
