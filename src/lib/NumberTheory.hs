module NumberTheory where

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

primes :: [Int]
primes = sieve [2..]
sieve :: Integral a => [a] -> [a]
sieve (p:ps) = p : sieve [ x |x<-ps, x `mod`p /= 0]
sieve [] = undefined 

factor :: Int -> [Int] -> Int
factor n (x:xs)
  | n `mod` x == 0 = x
  | otherwise = factor n xs
factor _ [] = undefined 

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..m]
  where
    m = floor $ sqrt $ fromIntegral x

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = loop1 n
  where
    loop1 n
      | even n = 2 : loop1 (n `div` 2)
      | otherwise = loop2 n 3
    loop2 n d
      | n < d * d = [ n | n /= 1 ]
      | otherwise = case n `divMod` d of
        (p, 0) -> d : loop2 p d
        _ -> loop2 n (d + 2)

-- ax + by = gcd(a, b)
-- extGcd a b = (gcd(a, b), x, y)

extGcd :: Int -> Int -> (Int, Int, Int)
extGcd a 0 = (a, 1, 0)
extGcd a b =
  let (g, a', b') = extGcd b $ a `mod` b
  in (g, b', a' - a `div` b * b')

-- xがaの逆元 (mod pで)
-- => ax ≡ 1 (mod p)
-- => ax - 1 ≡ 0 (mod p)
-- => ax - 1 がpで割り切れる
-- => ax - 1 = py
-- => ax + py = 1 となるx, yが存在する

inverseMod :: Int -> Int -> Maybe Int
inverseMod a modulus
  | g == 1 = Just $ if x < 0 then x + modulus else x
  | otherwise = Nothing
  where
    (g, x, _) = extGcd a modulus

-- x ≡ b1 (mod m1)
-- x ≡ b1 (mod m2)
-- chineseRem b1 m1 b2 m2 = (x, lcm(m1, m2))
chineseRem :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
chineseRem b1 m1 b2 m2 =
  if (b2 - b1) `mod` g /= 0
  then Nothing
  else
    let m = m1 * (m2 `div` g)
        t = (b2 - b1) `div` g * p `mod` (m2 `div` g)
    in Just (b1 + m1 * t, m)
  where
    (g, p, q) = extGcd m1 m2