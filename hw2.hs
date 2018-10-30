-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-019

import Data.Char

compareChars :: Char -> Char -> Char -> String
compareChars a b c
    | a == b && b == c             = "All  equal"
    | a == b || b == c || a == c   = "Two match"
    | otherwise                    = "All distinct"

combine :: Int -> Int -> Int -> Int
combine x y z
    | x >= 0 && x <= 9 && y >= 0 && y <= 9 && z >= 0 && z <= 9   = x * 100 + y * 10 + z
    | x == 0 && y >= 0 && y <= 9 && z >= 0 && z <= 9             = y * 10 + z
    | otherwise                                                  = -1

splitFloat :: Float -> (Integer, Float)
splitFloat num
    | num > 0    = (floor num, num - fromInteger (floor num))
    | num < 0    = (ceiling num, num - fromInteger (ceiling num))
    | otherwise  = (0, 0)

stdCost :: Int -> Int -> Bool -> Int
stdCost dev gb new
    | dev >= 1 && gb >= 0 && gb <= 4 && new == False   = 50 + (dev - 1) * 10
    | dev >= 1 && gb >= 0 && gb <= 4 && new == True    = 50 + (dev - 1) * 10 - 25
    | dev >= 1 && gb > 4 && new == False               = 50 + (dev - 1) * 10 + (gb - 4) * 15
    | dev >= 1 && gb > 4 && new == True                = 50 + (dev - 1) * 10 + (gb - 4) * 15 - 25
    | otherwise                                        = -1

powerCost :: Int -> Int -> Bool -> Int
powerCost dev gb new
    | dev >= 1 && (gb - 10 - dev * 2) < 0 && new == False    = 80 + (dev - 1) * 8
    | dev >= 1 && (gb - 10 - dev * 2) < 0 && new == True     = 80 + (dev - 1) * 8 - 25
    | dev >= 1 && (gb - 10 - dev * 2) >= 0 && new == False   = 80 + (dev - 1) * 8 + (gb - 10 - dev * 2) * 20
    | dev >= 1 && (gb - 10 - dev * 2) >= 0 && new == True    = 80 + (dev - 1) * 8 + (gb - 10 - dev * 2) * 20 - 25
    | otherwise                                              = -1

bestPlan :: Int -> Int -> String
bestPlan dev gb
    | dev == 1 && gb < 6 || dev < 9 && gb == 5 || dev < 16 && gb < 5                        = "Standard"
    | dev == 1 && gb > 6 || dev > 1 && gb > 5 || dev > 8 && gb == 5 || dev > 16 && gb > 4   = "Power user"
    | otherwise                                                                             = "Same cost"