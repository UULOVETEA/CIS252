---------------------------------------------------------------
--
--  Sample Solutions for Lab 9  (Spring 2017)
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

---------------------------------------------------------------
-- Your problems
---------------------------------------------------------------

replaceMe = error "replace with your own code"

-- :t one should give       Device
one = Gizmo 7 False         

-- :t two should give       (String, Int -> Bool)
two = ("hello",smallEven)     

-- :t three should give     String -> Bool
three s = length ('a':s) > 0  

-- :t four should give      [Char] -> [Bool]
four = map isUpper      

-- :t five should give      [Bool -> Device]
five = [Gizmo 7]        

-- :t six should give       (Bool,Int,Char) -> Char
six (b,i,c) = b && contrived i c 

-- or, without using contrived:
-- six (b,i,c) = b && i == length [] && isUpper c


-- :t seven should give     ((Char -> Bool) -> Char) -> Bool
seven f = isUpper (f isUpper)

-- :t eight should give     [[Double] -> [Shape]]
eight = [map (Rect 7)]    

-- :t nine should give      Char -> Bool -> Char
nine c b 
    | b = c 
    | otherwise = 'a'

-- :t ten should give       a -> b -> b
ten = curry snd

-- :t eleven should give    (String,[a]) -> [(Char,a)]
eleven (s,as) = zip (s++"abc") as

-- :t twelve should give    [(a,b) -> c] -> [a -> b -> c] 
twelve = map curry 

-- :t thirteen should give  (a -> b) -> (b -> c) -> a -> c
thirteen = flip (.)

-- :t fourteen should give  (Char -> Char -> Bool) -> Char -> Bool
fourteen f c = isUpper c && f c c 

