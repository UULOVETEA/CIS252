-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109

import Data.Char

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (n:ns) = n * myProduct ns

shout :: String -> String
shout ""      = []
shout (s:str) = toUpper s : shout str

zap :: Char -> String -> String
zap c [] = []
zap c (ch:cs)
    | c == ch   = zap c cs
    | otherwise = ch : zap c cs

pairUp :: [a] -> [(a,a)]
pairUp []       = []
pairUp (x:y:zs) = (x, y) : pairUp zs
pairUp (x:[])   = [(x,x)]

neighbors :: [a] -> [(a,a)]
neighbors []       = []
neighbors (x:y:zs) = (x, y) : neighbors (y:zs)
neighbors (x:[])   = []

bag1, bag2, bag3 :: [(Char, Int)]
bag1 = [('z', 1), ('e', 2), ('k', 1)]
bag2 = [('y', 2), ('a', 1), ('n', 1), ('c', 1), ('e', 1)]
bag3 = [('j', 1), ('o', 1), ('u', 1), ('l', 1), ('e', 1)]

bagCount :: [(Char, Int)] -> Int
bagCount [] = 0
bagCount ((a, b):bag) = b + bagCount bag

addToBag :: Char -> [(Char, Int)] -> [(Char, Int)]
addToBag ch []  = [(ch, 1)]
addToBag ch ((a, b):bag)
    | ch == a   = (a, b+1) : bag
    | otherwise = (a, b) : addToBag ch bag

removeFromBag :: Char -> [(Char, Int)] -> [(Char, Int)]
removeFromBag ch [] = []
removeFromBag ch ((a, b):bag)
    | ch /= a   = (a, b) : removeFromBag ch bag
    | b == 1    = bag
    | otherwise = (a, b-1) : bag