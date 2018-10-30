-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109

squarePairs :: Int -> Integer -> [(Integer, Integer)]
squarePairs n i
    | n <= 0    = []
    | otherwise = (i, i*i) : squarePairs (n-1) (i+1)

countDownBy :: Int -> Int -> Int -> [Int]
countDownBy m n diff
    | m < n     = []
    | otherwise = m : countDownBy (m-diff) n diff

steps :: Int -> Int -> [[Int]]
steps m n = helper m (helper m n)
    where
      helper i item
          | i > n     = []
          | otherwise = countUp m i : helper (i+1) item

countUp :: Int -> Int -> [Int]
countUp m n
    | m>n       = []
    | otherwise = m : countUp (m+1) n

indexChar :: Int -> Int -> Char -> String
indexChar n i c
    | n <= 0         = []
    | i < 1 || n < i = c : indexChar (n-1) i c
    | otherwise      = helper 1 i c
    where
      helper :: Int -> Int -> Char -> String
      helper x y z 
          | x > n     = []
          | x == i    = '!' : helper (x+1) y z
          | otherwise = z : helper (x+1) y z

diag :: Int -> Char -> [String]
diag n c = helper 1 (helper 1 c)
    where
      helper i item
          | i > n     = []
          | otherwise = indexChar n i c : helper (i+1) item