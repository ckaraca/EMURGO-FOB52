


-- 4 steps:
-- 1: Multiply every even position from the end by 2
-- 2: Add the 2 digits of any double digit number
-- 3: Add all the digits in the list
-- 4: Check if the result is divisible by 10

{-
addToEnd [1, 2, 3, 4, 5] 6
1 : (2 : (3 : (4 : (5 : (6 : [])))))

addToEnd [2, 3, 4, 5] 6
2 : (3 : (4 : (5 : (6 : []))))

addToEnd [3, 4, 5] 6
3 : (4 : (5 : (6 : [])))

addToEnd [4, 5] 6
4 : (5 : (6 : []))

addToEnd [5] 6
5 : (6 : [])

addToEnd [] 6
6 : []
-}

addToEnd :: [Int] -> Int -> [Int]
addToEnd [] y = [y]
addToEnd (x:xs) y = x : (addToEnd xs y)

{-
[1, 2, 3]
[3, 2, 1]

reverseList [1, 2, 3]
addToEnd [3, 2] 1

reverseList [2, 3]
addToEnd [3] 2

reverseList [3]
addToEnd [] 3 == [3]

-}

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = addToEnd (reverseList xs) x

mulEvenBeg2 :: [Int] -> [Int]
mulEvenBeg2 [] = []
mulEvenBeg2 [x] = [x]
mulEvenBeg2 (x:y:zs) = x : (2 * y) : mulEvenBeg2 zs

-- Logic for step 1
-- | Multiplies every even position from the end by 2
mulEvenEnd2 :: [Int] -> [Int]
mulEvenEnd2 ls = reverseList (mulEvenBeg2 (reverseList ls))

-- | Flatten a single double digit number
-- DISCLAIMER: ONLY WORKS FOR NUMBERS BELOW 18
flattenNum :: Int -> Int
flattenNum x =
    if x < 10
    then x
    else x - 9

-- Logic for step 2
-- | Flatten all double digit numbers
-- DISCLAIMER: ALL NUMBERS SHOULD BE BELOW 18
flattenNumInList :: [Int] -> [Int]
flattenNumInList [] = []
flattenNumInList (x:xs) = flattenNum x : flattenNumInList xs

-- Logic for step 3
-- | Add all the digits in the list
sumAllNumbersInList :: [Int] -> Int
sumAllNumbersInList [] = 0
sumAllNumbersInList (x:xs) = x + sumAllNumbersInList xs

-- Logic for step 4
-- | Check if the result is divisible by 10
checkIfDiv10 :: Int -> Bool
checkIfDiv10 x = x `mod` 10 == 0

-- | Validates a credit card number
testCard :: [Int] -> Bool
testCard ls =
    checkIfDiv10 (sumAllNumbersInList (flattenNumInList (mulEvenEnd2 ls)))
