-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109:q

roundEm :: [Float] -> [Integer]
roundEm rs = map (\x -> round x) rs

getLengths :: [String] -> [(String, Int)]
getLengths strs = map (\x -> (x, length x)) strs

grabOdds :: [Integer] -> [Integer]
grabOdds nums = filter (\x -> odd x) nums

negatives :: [(Char, Int)] -> [(Char, Int)]
negatives ps = filter (\(x, y) -> y < 0) ps

mystery :: Int -> [Int] -> [Int]
mystery k qs = [ q*100 | q <- qs, q < 3*k]

mystery2 :: Int -> [Int] -> [Int]
mystery2 k qs = map (\x -> x*100 ) (filter (\y -> y < 3*k) qs)