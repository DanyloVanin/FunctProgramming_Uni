{-# OPTIONS_GHC -Wall #-}
module Vanin02 where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl = foldl (+) 0
  
-- Задача 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr  = foldr (*) 1

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert  = foldl insert []

insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert (x:xs) v | x < v = x : insert xs v
 | otherwise = v : x : xs
 
-- Задача 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = [ i | i<-[0..length xs - 1], p (xs !! i)]

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- Задача 7  -----------------------------------------
noDigits :: String -> String
noDigits = filter (not . flip elem ['0'..'9'])

-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length [x | x<-ps, x v]

-- Задача 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate nextPasLine [1]

nextPasLine :: [Integer] -> [Integer]
nextPasLine xs = zipWith (+) (0:xs) (xs ++ [0])
-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]

