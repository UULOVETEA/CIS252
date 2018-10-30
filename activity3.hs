import Data.Char

zs :: [Integer]
zs = [8, 12, 67, 31, 2]

lon  :: [Integer]
lon = [ y+1 | y <- zs, even y, 2*y < 20 ]

w :: Int
w = 5

mystery :: [a] ->Char
mystery [] = 'R'
mystery (p:ps) = 'S'
mystery (p:q:qs) = 'G'

{-(:) :: a -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
concat :: [[a]] -> [a]
even, odd :: Integral a => a -> Bool
length :: [a] -> Int
isUpper, isLower :: Char -> Bool
toUpper, toLower :: Char -> Char
fst :: (a,b) -> a
snd :: (a,b) -> b
zip :: [a] -> [b] -> [(a,b)]
unzip :: [(a,b)] -> ([a],[b])-}