import Data.Char
import Text.Printf

main :: IO ()
main = do
  k <- readLn
  s <- getLine -- Takahashi
  t <- getLine -- Aoki
  let s' = map digitToInt $ take 4 s
  let t' = map digitToInt $ take 4 t
  printf "%f\n" $ solve k s' t'

solve :: Int -> [Int] -> [Int] -> Double
solve k s t = sum $ map fst $ filter snd $ zip probs winOrLoose
  where
    secrets = [(s5, t5) | s5 <- [1..9], t5 <- [1..9]]
    cards = map (\(s5, t5) -> (s5:s, t5:t)) secrets
    probs = map (prob k) cards
    winOrLoose = map winsFirst cards
     
prob :: Int -> ([Int], [Int]) -> Double
prob k (s, t) = if remainsS5 <= 0 || remainsT5 <= 0 then 0.0 else sp * tp
  where
    s5 = head s
    t5 = head t
    remainsS5 = fromIntegral $ k - count (tail s) s5 - count (tail t) s5
    remainsT5 = fromIntegral $ k - count s t5 - count (tail t) t5
    k' = fromIntegral k
    sp = remainsS5 / fromIntegral (9 * k - 8)
    tp = remainsT5 / fromIntegral (9 * k - 9)

score :: [Int] -> Int
score cards = sum $ map (\i -> i * 10 ^ count cards i) [1..9]

count cards i = length $ filter (==i) cards
     
winsFirst :: ([Int], [Int]) -> Bool
winsFirst (s, t) = score s > score t