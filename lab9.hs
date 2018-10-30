-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-019

---------------------------------------------------------------
--
--  Starting file for Lab 9  (Spring 2017)
--
---------------------------------------------------------------


import Data.Char   -- load Data.Char module to get definition of isUpper

data Shape = Circle Float
           | Rect Double Double
             deriving (Show,Eq)

data Device = Gizmo Int Bool
            | Widget Float Char
             deriving (Show,Eq)

smallEven :: Int -> Bool
smallEven x = even x && x < 10

contrived :: Int -> Char -> Bool
contrived x c = isUpper c && odd x



---------------------------------------------------------------
-- The examples from the lab writeup
---------------------------------------------------------------

--
-- Example 1: [Int]
--
-- :t ansEx2 gives the type [Int]
ansEx1 = [length "abc"]

--
-- Example 2: [Float]
--
-- Two versions:
--   :t ansEx2   gives the type [Float]
--   :t ansEx2'   gives the type [Float]
ansEx2 = getList [] 
  where
    getList vs = snd (map Circle vs, vs)

ansEx2' = [x]
  where
    Circle x = Circle 3.21

--
-- Example 3: Bool -> Int
--
-- Two versions:
--   :t ansEx3 gives the type Bool -> Int
--   :t ansEx3' gives the type Bool -> Int
ansEx3 = sample
  where
    sample b
        | b         = length []
        | otherwise = 15

ansEx3' b 
    | b         = length []
    | otherwise = 15

--
-- Example 4: [Float] -> [Shape]
--
--   :t ansEx4 gives the type [Float] -> [Shape]
ansEx4 = map Circle

helperFor3 "Orange" = True
helperFor6 (False, 7, 'S') = 's'
helperFor9 's' False = 'S'
helperFor11 ("Orange", [x]) = zip "Orange" [x]

---------------------------------------------------------------
-- Your problems
---------------------------------------------------------------

replaceMe = error "replace with your own code"

-- :t one should give       Device
one = Gizmo 7 False         -- replace with your answer

-- :t two should give       (String, Int -> Bool)
two = ("Orange", smallEven) -- replace with your answer

-- :t three should give     String -> Bool
three = helperFor3          -- replace with your answer

-- :t four should give      [Char] -> [Bool]
four = map isAlpha          -- replace with your answer

-- :t five should give      [Bool -> Device]
five = [Gizmo 7]            -- replace with your answer

-- :t six should give       (Bool,Int,Char) -> Char
six = helperFor6            -- replace with your answer

-- :t seven should give     ((Char -> Bool) -> Char) -> Bool
seven = replaceMe           -- replace with your answer

-- :t eight should give     [[Double] -> [Shape]]
eight = [map (Rect 7)]      -- replace with your answer

-- :t nine should give      Char -> Bool -> Char
nine = helperFor9           -- replace with your answer

-- :t ten should give       a -> b -> b
ten = (\ x y -> y)          -- replace with your answer

-- :t eleven should give    (String,[a]) -> [(Char,a)]
eleven = helperFor11        -- replace with your answer

-- :t twelve should give    [(a,b) -> c] -> [a -> b -> c] 
twelve = map curry          -- replace with your answer

-- :t thirteen should give  (a -> b) -> (b -> c) -> a -> c
thirteen = replaceMe        -- replace with your answer

-- :t fourteen should give  (Char -> Char -> Bool) -> Char -> Bool
fourteen replaceMe          -- replace with your answer

