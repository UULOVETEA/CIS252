-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-019

simple :: Integer -> Integer -> Integer
simple a b = a + 3*b

twoForOne :: Integer -> Integer -> Integer
twoForOne items price
    | items <= 0 || price <= 0 = 0
    | even items = (div items 2) * price
    | otherwise  = (1 + (div items 2)) * price

price :: Integer -> Integer -> Integer
price big tiny
    | big < 0 || tiny < 0 = 0
    | big + tiny < 10 = (big * 850 + tiny * 625)
    | otherwise = (big * 795 + tiny * 550) 