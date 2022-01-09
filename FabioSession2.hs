factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
  | mod n 2 == 0 = div n 2
  | otherwise    = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 20
foo 1
  | 7 > 5     = 40
  | otherwise = 60
foo n
  | n < 0        = 0
  | mod n 2 == 0 = 80
  | otherwise    = 100

myNetWorth :: (Int, String)
myNetWorth = (20, "Rupees")

sumPair :: (Int, Int)  -> Int
sumPair (a, b) = a + b

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

result :: Int
result = sum3 3 17 8

oneToTen :: [Integer]
oneToTen = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

oneToTenRange :: [Integer]
oneToTenRange = [1..10]

evensBetween1And10 :: [Integer]
evensBetween1And10 = [2, 4..10]

theForceIsChar :: [Char]
theForceIsChar = ['S','t','r','o','n','g']

theForceIsString :: String
theForceIsString = "Strong"

emptyList = []
listWithOneElement = 1 : []
listWithTwoElements = 1 : 2 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []       = []
sumEveryTwo (x:[])   = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

myTake :: Integer -> [Integer] -> [Integer]
myTake _ []       = []
myTake 1 (x : xs) = x : []
myTake n (x : xs) 
  | n < 1         = []
  | otherwise     = x : myTake (n - 1) xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

