--Credit card test

testCard :: [Int] -> Bool
testCard ls = boolResult (sumList(flattenNumInList (mulEvenEnd2 ls)))

addToEnd :: [Int] -> Int -> [Int]
addToEnd [] y = [y]
addToEnd (x:xs) y = x : (addToEnd xs y)

--Revert a list
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = addToEnd (reverseList xs) x

mulEvenBeg2 :: [Int] -> [Int]
mulEvenBeg2 [] = []
mulEvenBeg2 [x] = [x]
mulEvenBeg2 (x:y:zs) = x : (2 * y) : mulEvenBeg2 zs

--Multiply * 2 number of list 2 by 2
mulEvenEnd2 :: [Int] -> [Int]
mulEvenEnd2 ls = reverseList (mulEvenBeg2 (reverseList ls))

--Disclaimer: Only works for numbers in range 10 to 18
sumNumberListGreaterThan9 :: Int -> Int
sumNumberListGreaterThan9 x = 
                          if x < 10 
                          then x
                          else x-9

flattenNumInList :: [Int] -> [Int]
flattenNumInList [] = []
flattenNumInList (x:xs) = sumNumberListGreaterThan9 x : flattenNumInList xs

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs


boolResult :: Int -> Bool
boolResult x = x'mod' 10 == 0
