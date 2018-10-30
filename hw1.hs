-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-019

between :: Int -> Int -> Int -> Bool
between m y z = (m > y) && (y < z)

xor :: Bool -> Bool -> Bool
xor e1 e2 = (e1 == True) && (e2 == False) || (e1 == False) && (e2 == True)

convertDtoF :: Float -> Float
convertDtoF temp = 212.0 - 6/5 * temp

convertFtoD :: Float -> Float
convertFtoD temp = (212.0 - temp) * 5/6