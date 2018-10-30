-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109

one = [200+x | x <- [1..10]]

two = [odd x | x <- [1..10]]

three = [1+x | x <- [1..10], x < 8]

four = [[4*x] | x <- [1..10], x < 6]

five = [(5*x, (5*x) < 20) | x <- [1..10], x < 6]

six = [(10*(2*x-1), 10*(2*x)) | x <- [1..10], x < 6]

seven = [[11-x] | x <- [1..10]]

eight = [[12-2*x] | x <- [1..10], x < 7]

nine = [[3..2*x+2] | x <- [1..10], x < 5]

ten = [35 - 5*x | x <- [1..10], x < 8]

