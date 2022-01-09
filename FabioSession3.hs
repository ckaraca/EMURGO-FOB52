takeLastDigit :: [Integer] -> Integer
takeLastDigit []       = -1
takeLastDigit (x : []) = x
takeLastDigit (x : xs) = takeLastDigit xs

dropLastDigit :: [Integer] -> [Integer]
dropLastDigit []       = []
dropLastDigit (x : []) = []
dropLastDigit (x : xs) = x : dropLastDigit xs

reverseDigits :: [Integer] -> [Integer]
reverseDigits [] = []
reverseDigits (x : []) = x : []
reverseDigits (x : xs) = reverseDigits xs ++ [x]

doubleDigitAtOdd :: Integer -> Integer -> Integer
doubleDigitAtOdd pos digit
  | mod pos 2 == 0 = digit
  | otherwise      = digit * 2

doubleDigitsAtOdds :: Integer -> [Integer] -> [Integer]
doubleDigitsAtOdds _ [] = []
doubleDigitsAtOdds pos (digit : []) = doubleDigitAtOdd pos digit : []
doubleDigitsAtOdds pos (digit : digits) = doubleDigitAtOdd pos digit : doubleDigitsAtOdds (pos + 1) digits

doubleDigitsAtOddPositions :: [Integer] -> [Integer]
doubleDigitsAtOddPositions []     = []
doubleDigitsAtOddPositions digits = doubleDigitsAtOdds 1 digits

subtractNine :: [Integer] -> [Integer]
subtractNine [] = []
subtractNine (x : xs)
  | x > 9     = (x - 9) : subtractNine xs
  | otherwise = x : subtractNine xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = x + sumDigits xs

validateCC :: [Integer] -> Bool
validateCC [] = False
validateCC cc = if mod ((+) (takeLastDigit cc) $ sumDigits $ subtractNine $ doubleDigitsAtOddPositions $ reverseDigits $ dropLastDigit cc) 10 == 0 then True else False

sampleCC :: [Integer]
sampleCC = [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]