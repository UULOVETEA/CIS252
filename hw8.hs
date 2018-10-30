-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-019

import BinaryTrees

height :: BTree a -> Int
height Empty                = -1
height (BNode x left right) = 1 + max (height left) (height right)

autumn :: BTree a -> BTree a
autumn Empty                 = Empty
autumn (BNode x Empty Empty) = Empty
autumn (BNode x left right)  = BNode x (autumn left) (autumn right)

full :: BTree a -> Bool
full (BNode x Empty Empty) = True
full (BNode x Empty _)     = False
full (BNode x _ Empty)     = False
full (BNode x left right)  = (full left) && (full right)

mirror :: BTree a -> BTree a
mirror (BNode x left right) = BNode x right left

mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f Empty                = Empty
mapTree f (BNode x left right) = BNode (f x) (mapTree f left) (mapTree f right) 

depthVals :: Int -> BTree a -> [a]
depthVals n Empty = []
depthVals n (BNode x left right)
    | n == 0    = [x]
    | n < 0     = []
    | otherwise = depthVals (n-1) left ++ depthVals (n-1) right

minValue :: Ord a => a -> BTree a -> a
minValue val Empty                 = val
minValue val (BNode x Empty Empty) = x
minValue val (BNode x left right)  = minimum (x : (minValue val left) : (minValue val right) : [])

onPath :: Path -> BTree a -> [a]
onPath path Empty                       = []
onPath [] (BNode x left right)          = [x]
onPath (Lft:rest) (BNode x left right)  = [x] ++ onPath left
onPath (Rght:rest) (BNode x left right) = [x] ++ onPath right