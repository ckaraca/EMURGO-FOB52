-- Take n element of the List
takeElement :: Int → [Int] -> [Int]
takeElement _ [] = []
takeElement n _ | n <= 0 = []
takeElement n (x:xs) = x : takeElement (n-1) xs

-- Get List Length (to get the last element)
intListLength :: [Integer] → Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs
intListLength (_:xs) = 1 + intListLength xs

addToEnd :: [Int] -> Int -> [Int]
addToEnd [] y = [y]
addToEnd (x:xs) y = x : addToEnd xs y

-- Reverse the List
reverseList :: [Int] -> [Int] -> [Int]
reverseList [] = []
reverseList (_:xs) = addToEnd (reverseList xs) x

-- Multiply by 2 Even numbers
doubleEven :: [Int] -> [Int]
doubleEven [] = []
doubleEven [x] = [x]
doubleEven (x:y:xs) = x : (2 * y) : doubleEven xs

doubleEvenFromEnd :: [Int] -> [Int]
doubleEvenFromEnd :: [Int] -> [Int]
doubleEvenFromEnd ls = reverseList (doubleEven (reverseList ls))

-- Sum Digits if value > 9
sumDigits :: Int -> Int
sumDigits x 
  | x > 9 = x - 9
  | otherwise x

-- Sum digits > 9 in the List
sumDoubleDigit :: [Int] -> [Int]
sumDoubleDigit [] = []
sumDoubleDigit (x:xs) = sumDigits x : sumDoubleDigits xs

sumAllDigits :: [Int] -> Int
sumAllDigits [] = 0
sumAllDigits (x:xs) = x + sumAllDigits xs

validCred :: [Int] -> Bool
validCred [] = []
validCred (x:xs) 
  | sumAllDigits (sumDoubleDigit (doubleEven xs)) 'Mod' 10 == 0  = True
  | otherwise False

