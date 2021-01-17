import Data.Array

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "Yes" else "No"

solve :: String -> Bool
solve "8" = True
solve [_] = False
solve [s1, s2] = read [s1, s2] `mod` 8 == 0 || read [s2, s1] `mod` 8 == 0
solve s = any props candidates
  where
    sDigits = digits s
    candidates = [digits s | n <- [112,120..992], 
                             let s = show n, 
                             '0' `notElem` s]
    props candi = all (\d -> sDigits ! d >= candi ! d) ['1'..'9']

digits :: String -> Array Char Int
digits s = accumArray (+) 0 ('1', '9') [(c, 1) | c <- s]
