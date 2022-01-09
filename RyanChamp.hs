-- Session 2 Exercise
take' :: Int -> [Int] -> [Int]
take' _ [] = []     -- I originally missed this case
take' a (x:xs)
  | a <= 0 = []
  | otherwise = x : take' (a - 1) xs

-- Session 3 - Mod10

-- Turn an int into a list of digits
intToDigitList :: Int -> [Int]

-- Sum the elements of the list
sum' :: [Int] -> Int

-- reverse an int list
reverse' :: [Int] -> [Int]

-- Multiply Even list elements by 2 from the end
doublize :: [Int] -> [Int]

-- Validate credit card
mod10 :: [Int] -> Bool