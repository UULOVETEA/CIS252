-- Name: Xiaomeng Cao
-- Email: xcao07@syr.edu
-- Section: CST 1-109

----------------------------------------------------------------------
--    Stater File for HW6  (CIS 252: Spring 2017)
----------------------------------------------------------------------

-- possible pizza toppings
data Topping = Pepperoni | Onions | Ham | Mushrooms | Chicken
               deriving (Show)

-- sauces for pizzas and breadsticks
data Sauce = Tomato | Garlic | Pesto 
             deriving (Show)

-- sizes for pizzas and salads
data Size = Small | Large
            deriving (Show)

-- possible dressings for salads
data Dressing = Ranch | Greek | Caesar | None
                deriving (Show)

-- items on the menu for purchase
data MenuItem = Breadsticks Sauce
              | Salad Size Dressing
              | Pizza Size Sauce [Topping]
                deriving (Show)


----------------------------------------------------------------------
--  Do not make alterations to the code above this line.  All of your
--  code should appear below this comment.
----------------------------------------------------------------------

breadsticks1, salad1, pizza1, pizza2 :: MenuItem

breadsticks1 = Breadsticks Pesto
salad1       = Salad Large Caesar
pizza1       = Pizza Small Garlic []
pizza2       = Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]

order1, order2 :: [MenuItem]

order1 = [Salad Large None, 
          Breadsticks Garlic, 
          Pizza Large Tomato []]
order2 = [Pizza Small Tomato [Pepperoni, Mushrooms, Ham, Onions], 
          Salad Small Ranch, 
          Salad Large Greek, 
          Pizza Large Pesto [Ham, Chicken], 
          Breadsticks Garlic]

toppingCost :: Size -> Topping -> Float
toppingCost Small Chicken = 2.25
toppingCost Large Chicken = 3.25
toppingCost Small _       = 1.50
toppingCost Large _       = 2.25

sauceCost :: Size -> Sauce -> Float
sauceCost Small Pesto = 13.25
sauceCost Large Pesto = 16.50
sauceCost Small _     = 11.50
sauceCost Large _     = 14.00

cutCalories :: MenuItem -> MenuItem
cutCalories (Breadsticks Pesto)     = Breadsticks Pesto
cutCalories (Salad Large Caesar)    = Salad Small None
cutCalories (Pizza Small Garlic []) = Pizza Small Garlic []
cutCalories (Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]) = Pizza Small Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]

willEat :: MenuItem -> Bool
willEat (Breadsticks Pesto)        = False
willEat (Salad Large Caesar)       = True
willEat (Pizza Small Garlic [])    = True
willEat (Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]) = False
willEat (Pizza _ Pesto [_])        = False
willEat (Pizza _ _ [Mushrooms, _]) = False

price :: MenuItem -> Float
price (Breadsticks Pesto)     = 4.25
price (Salad Large Caesar)    = 9.5
price (Pizza Small Garlic []) = 11.50
price (Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]) = 23.0

numToppings :: [MenuItem] -> Int
numToppings [(Salad Large Caesar), (Breadsticks Pesto)] = 0
numToppings [(Pizza Small Garlic []), 
             (Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms])] = 4
numToppings [Salad Large None, 
             Breadsticks Garlic, 
             Pizza Large Tomato []] = 0
numToppings [Pizza Small Tomato [Pepperoni, Mushrooms, Ham, Onions], 
             Salad Small Ranch, 
             Salad Large Greek, 
             Pizza Large Pesto [Ham, Chicken], 
             Breadsticks Garlic] = 6

promotion :: [MenuItem] -> Float
promotion [Salad Large None, 
           Breadsticks Garlic, 
           Pizza Large Tomato []] = 23.5
promotion [(Breadsticks Pesto)] = 4.25
promotion [(Pizza Small Garlic []), (Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms])] = 23.0