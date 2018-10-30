import Data.Char

countUp  ::  Int -> Int -> [Int]
countUp m n
    | m > n       = []
    | otherwise   = m : countUp (m+1) n

countUpBy :: Int -> Int -> Int -> [Int]
countUpBy m n diff
    | m > n       = []
    | otherwise   = m : countUpBy (m+diff) n diff

powers :: Integer -> Integer -> [Integer]
powers n x
    | n <= 0       = []
    | otherwise   = helper 1 x
    where
      helper :: Integer -> Integer -> [Integer]
      helper i val
          | i > n       = []
          | otherwise   = val : helper (i+1) (val*x)


series :: Int -> a -> [a]
series n item
  | n <= 0    = []
  | otherwise = item : series (n-1) item

strings :: Int -> Char -> [String]
strings n item
    | n <= 0  = []
    | otherwise   = helper 1
    where
      helper :: Int -> [String]
      helper i
          | i > n = []
          | otherwise = (series i item) : helper (i+1)

n, x :: Integer
n = 7
x = 2
helper :: Integer -> Integer -> [Integer]
helper i val
    | i > n       = []
    | otherwise   = val : helper (i+1) (val*x)

query :: [a] -> Float
query [] = 5000.00
query (q:_:ts) = 30.0 + query ts
query _ = 100.00

exam1 :: Int -> Int -> Int -> Bool
exam1 x y z
    | x > 0 && y > 0 = (x*x+y*y) == z*z
    | otherwise = False

triAn :: Float -> Float -> Float -> Float
triAn x y z
    | x < 0.0 || y<0.0 || z<0 = 0.0
    | x+y<z || y+z<x || x+z<y = 0
    | otherwise = (x+y+z)/2

data Music = Sone Char
           | Ditty Int [Integer]
           | Melody Bool

honk :: Music -> Integer
honk (Ditty w (t:ts))
    | even w  = t
    | otherwise = 1 + honk (Ditty (w+1) ts)
honk _ = 400

baa :: (Int -> Int -> Bool) -> Int -> [Int]
baa f x 
    | f x 7 = [x]
    | otherwise = x : baa f (x+1)

hiss :: Int -> Int
hiss j = j * 100

oink :: Int -> Int
oink z = z+300

uno (_:_:k:_) = k

puzzle :: Int -> [Int] -> [Int]
puzzle w zs = [20 - z| z<- zs , w>3*z]

enigma :: (Char -> Int, Char) -> Bool
enigma (g,c) = isLower c || odd (g c)

enigma' :: (Char -> Int) -> Char -> Bool
enigma' g c = isLower c || odd (g c)

either :: ((a -> Bool), (a -> Bool)) -> (a -> Bool)
either (p, q) w = p w || q w