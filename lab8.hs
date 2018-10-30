-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST1-109

uncurry3Test :: Int -> Int -> Int -> Int
uncurry3Test a b c = a + 3*b + c

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

simple :: Int -> Int -> Int
simple a b = a + 3*b

compressFun :: (a -> a -> b) -> (a -> b)
compressFun f x = f x x

doTwo :: ((a -> b), (a -> c)) -> (a -> (b, c))
doTwo (f, g) x = (f x, g x)