takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN _ [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- I just used the type of take

module Lib
    ( someFunc
    , checkCard
    , toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , getCard
    , cC
    , filterDigits
    , p
    ) where

import Data.Char (digitToInt)
import Data.Char (isDigit)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


getCard :: IO ()
getCard = do
           putStr "Please Enter Card Number (no spaces): " 
           card <- getLine
           
           if length card == 16 && ((checkCard card) `mod` 10 == 0)
              then
                putStrLn ("Valid 16 digit card number entered: " ++ card)
              else
                putStrLn ("Invalid card number: " ++ card)

-- Takes a character and returns a Bool depending on if it is a digit between 0 and 9
p :: Char -> Bool
p x =
  if "0" <= [x] && [x] <= "9"
     then True
     else False

-- Take a String and parse it to remove any characters that are NOT digits
filterDigits :: [Char] -> [Char]
filterDigits [] = []
filterDigits (x:xs) = 
              if p x
              then x:filterDigits xs
              else filterDigits xs
                          
-- convert String to list of Int
toDigits :: [Char] ->  [Int]
toDigits =  map digitToInt

-- convert String to list of Int reversed
toDigitsRev :: [Char] -> [Int]
toDigitsRev n = reverse (toDigits n)

-- Double alternate values 
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther []      = []
doubleEveryOther (x:[])  = [x]
doubleEveryOther (x:y:zs) = x : (2*y) : doubleEveryOther zs

n = "4012888888881881"

-- checkCard :: Int
checkCard = sum . sumDigits . doubleEveryOther . toDigitsRev . filterDigits

sumDigits :: [Int] -> [Int]
sumDigits []        = []
sumDigits (x:xs) =
                  if x < 10
                  then x : sumDigits xs
                  else (x `quot` 10) : (x `mod` 10) : sumDigits xs






