testCard :: [Int] -> Bool

-- Reverse the list
reverseList :: {Int] -> [Int]


-- multiply every second digit from the end by 2
testCard  (x:xs)
    | xs == [] = testCard (2*x:xs)
