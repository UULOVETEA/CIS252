------------------------------------------------------------------------
-- series and altSeries were introduced in Jan 31 lecture
------------------------------------------------------------------------

--
-- series n item
--     returns a list with n copies of item
--
--  Example:  series 7 0  returns  [0,0,0,0,0,0,0]
--

series :: Int -> a -> [a]
series n item
  | n <= 0    = []
  | otherwise = item : series (n-1) item




--
-- altSeries :: Int -> a -> a -> [a]
--
-- altSeries n thing1 thing2
--    returns a list with n items, alternating between thing1 and thing2
--
--  Example:  altSeries 7 0 3  returns  [0,3,0,3,0,3,0]
--

altSeries :: Int -> a -> a -> [a]
altSeries n thing1 thing2
  | n <= 0    = []
  | otherwise = thing1 : altSeries (n-1) thing2 thing1




------------------------------------------------------------------------
-- Functions to introduce in lecture on Feb 2
------------------------------------------------------------------------

--
-- countUp :: Int -> Int -> [Int]
--   
--    countUp m n   
--         generates the list [m, m+1, m+2, ..., n]  (provided m <= n)
--         If m > n, returns the empty list
--
--  Example:  countUp 2 5   returns  [2,3,4,5]
--

countUp :: Int -> Int -> [Int]
countUp m n 
    | m > n     = []
    | otherwise = m : countUp (m+1) n


--
-- countUpBy :: Int -> Int -> Int -> [Int]
--   
--    countUpBy m n diff
--         generates the list [m, m+diff, m+2*diff, ..., x]  (provided m <= n),
--                where x is largest integer that does not exceed n
--
--         If m > n, returns the empty list
--
--  Example:  countUpBy 2 10 3  returns  [2,5,8]
--            countUpBy 2 17 3  reutrns  [2,5,8,11,14,17]
--

countUpBy :: Int -> Int -> Int -> [Int]
countUpBy m n diff
    | m > n     = []
    | otherwise = m : countUpBy (m+diff) n diff


--
-- powers :: Integer -> Integer -> [Integer]
--
--   powers n x 
--          generates the list [x^1,x^2,..., x^n]
--            (if n <= 0, returns empty list)
--        
--   Example: powers 7 2 returns  [2,4,8,16,32,64,128]
--            powers 5 10 returns [10,100,1000,10000,100000]
--

powers :: Integer -> Integer -> [Integer]
powers n x
    | n <= 0    = []
    | otherwise = helper 1 x
    where
      helper :: Integer -> Integer -> [Integer]
      helper i val
          | i > n     = []
          | otherwise = val : helper (i+1) (val*x)

  
  
--
--  strings n item
--       returns a list containing n strings: 
--             the i-th string contains i copies of item
--
--  Example:  strings 5 'S' returns  ["S","SS","SSS","SSSS","SSSSS"]
--

-- Note that this type is equivalent to:  Int -> Char -> [[Char]]
strings :: Int -> Char -> [String]
strings n item
    | n <= 0    = []
    | otherwise = helper 1
    where
      helper :: Int -> [String]
      helper i
          | i > n = []
          | otherwise = (series i item) : helper (i+1)

aaa :: Int -> Char -> [String]
aaa n item = helper 1 [item]
    where
      helper i y
          | i>n       = []
          | otherwise = y : helper (i+1) (item : y)


