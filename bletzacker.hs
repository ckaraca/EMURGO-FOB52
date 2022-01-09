-- | Reverses a list
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mulEven :: [Int] -> [Int]
mulEven [] = []
mulEven ([x]) = [x]
mulEven (x:y:zs) = x : (2*y) : mulEven zs

mulEvenEnd :: [Int] -> [Int]
mulEvenEnd ls = reverseList (mulEven  reverseList ls))

-- | Flatten a single double digit number
-- DISCLAIMER : ONLY WOKS FOR NUMBERS IN RANGE 10 to 18
flattenNum :: int -> Int

-- | Validates a credit card number
testCard :: [Int] -> Bool