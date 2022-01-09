revM :: [Int] -> [Int]
revM [] = []
revM x : xs = (revM xs) ++ [x]

mulSeconds :: [Int] -> [Int]
mulSeconds []         = []
mulSeconds x : []     = x : []
mulSeconds x : y : xs = x : (2 * y) : (mulSeconds xs)

mulSecondsFromLast :: [Int] -> [Int]
mulSecondsFromLast = revM . mulSeconds

doubleValues :: [Int] -> [Int]
doubleValues xs = map (* 2) xs

stepOne :: [Int] -> [Int]
stepOne = takeSecondsFromLast . doubleValues

addDigits :: Int -> Int
addDigits x
          | x >= 10   = (x `mod` 10) + (x `div` 10)
          | otherwise = x

stepTwo :: [Int] -> [Int]
stepTwo = map addDigits

stepThree :: [Int] -> Int
stepThree = sum

stepFour :: Int -> Boolean
stepFour x = x `mod` 10 == 0

luhn :: [Int] -> Boolean
luhn = stepOne . stepTwo . stepThree . stepFour