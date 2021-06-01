
import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = do
  css <- replicateM 3 $ map read . words <$> getLine
  putStrLn $ if isSatisfied css then "Yes" else "No"

isSatisfied :: [[Int]] -> Bool
isSatisfied cs = not $ null [1|
    a1 <- [0..ca1],
    a2 <- [0..ca2],
    a3 <- [0..ca3],
    let b1 = cs !! 0 !! 0 - a1
        b2 = cs !! 0 !! 1 - a1
        b3 = cs !! 0 !! 2 - a1
        ab = [a+b|a <- [a1,a2,a3], b <- [b1, b2, b3]],
    ab == concat cs
    ]
  where
    ca1 = maximum $ cs' !! 0
    ca2 = maximum $ cs' !! 1
    ca3 = maximum $ cs' !! 2
    cs' = transpose cs
