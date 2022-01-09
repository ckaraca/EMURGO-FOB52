-- | Takes n elements of a list given in parameter
takeElement :: Int -> [Int] -> [Int]
takeElement _ [] = []
takeElement n (x:xs) 
    | n <=0 = []
    | null xs = [x]
    | otherwise = x : takeElement (n-1) xs

takeElem :: Int -> [Int] -> [Int]
takeElem n _ | n <= 0 = []
takeElem n (x:xs) = x : takeElem (n - 1) xs
takeElem _ [] = []
    

takeElement2 :: [Int]
takeElement2 = takeElement 2 [1,2,3,4,5]

takeElementNegative :: [Int]
takeElementNegative = takeElement (-1) [1,2,3,4,5]

takeElement6 :: [Int]
takeElement6 = takeElement 6 [1,2,3,4,5]

takeElementError :: [Int]
takeElementError = takeElement 6 []


--- Credit card exercice

-- 1 | Revert a list 

addToEnd -> [Int] -> Int -> [Int]
addToEnd [] y -> [y]
addToEnd (x:xs) y = x : addToEnd xs y 

reverseList :: [Int] -> [Int]
reverseList  [] = []
reverseList  (x:xs) = addToEnd (reverseList xs) x

-- 2 Multiply even index by 2
mulEvenBy2 :: [Int] -> [Int]
mulEvenBy2 [] -> []
mulEvenBy2 [x] -> [x]
mulEvenBy2 (x:y:zs) = x : (2 * y) : mulEvenBy2 zs

mulEvenEnd2 :: [Int] -> [Int]
mulEvenEnd2 ls = reverseList (mulEvenBy2 (reverseList))

-- 4 | Flatten a single double digit number
flattenNum :: Int -> Int
flattenNum x = 
    if x < 10 
    then x
    else x - 9

flattenNumInList :: [Int] -> [Int]
flattenNumInList [] -> []
flattenNumInList (x:xs) = flattenNum x : flattenNumInList xs

-- 6 | Sum every number of list
sumElementsList :: [Int] -> Int
sumElementsList [] = 0
sumElementsList (x:xs) = x + sumElementsList xs

-- 7 | Check if number is divisible by 10
checkMod10 :: Int -> Bool
checkMod10 x = x `mod` 10 == 0


testCard :: [Int] -> Bool
testCard ls = checkMod10 (sumElementsList(flattenNumInList(mulEvenEnd2 ls)))
