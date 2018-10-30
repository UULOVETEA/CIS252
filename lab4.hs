-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109

cycle3 :: Int -> Integer -> Integer -> Integer -> [Integer]
cycle3 n item1 item2 item3
    | n == 0      = []
    | n > 0       = item1 : cycle3 (n-1) item2 item3 item1
    | otherwise   = error "cycle3: requires nonnegative input"

switchback :: Int -> Integer -> Integer -> [Integer]
switchback n item1 item2
    | n == 0            = []
    | n > 0 && odd n    = item1 : switchback (n-1) item2 item1
    | n > 0 && even n   = item1 : switchback (n-1) item1 item2
    | otherwise         = error "switchback: requires nonnegative input"
