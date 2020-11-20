{-# OPTIONS_GHC -Wall #-}
module Vanin01 where

-- Задача 1 ----------------------------------------- 
power3 :: [Integer]
power3 = [x^3 | x<-[1,2..]]

-- Задача 2 ----------------------------------------- 
toPower3 :: [Integer]
toPower3 = [3^x | x<-[1,2..]]

-- Задача 3 ----------------------------------------- 
-- допоміжна функція 
sm :: [Integer] -> Integer 
sm xs = if null xs then 0 else head xs + sm (tail xs) 
-- основна
sumPower3 :: Integer -> Integer
sumPower3 a = sm ([3^x | x<-[1..a]])

-- Задача 4 ----------------------------------------- 
sumPower :: Integer -> Integer -> Integer
sumPower m n = sm ([m^i | i<-[1..n]])

-- Задача 5 ----------------------------------------- 
-- допоміжна функція 
countLessThan :: Int -> [Int] -> Int
countLessThan m xm = length [ x | x<-xm, x<m]
-- основна
lessMe :: [Int] -> [Int]
lessMe xs = [(countLessThan x xs)| x<-xs] 
 
-- Задача 6 -----------------------------------------  
-- допоміжна функція 1
timesInList :: Int -> [Int] -> Int
timesInList m xm = length [ x | x<-xm, x==m]
-- допоміжна функція 2
contains :: Int -> [Int] -> Bool
contains n xs = (timesInList n xs) > 0
-- допоміжна функція 3
removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = if null xs then [] else (if (contains (head xs) (tail xs)) then (removeDuplicates (tail xs)) else ((head xs) : (removeDuplicates (tail xs))))
-- основна
frequency :: [Int] -> [(Int,Int)]
frequency xs = [(x, (timesInList x xs)) | x<-(removeDuplicates xs)]

-- Задача 7 ----------------------------------------- 
hailstone :: Int -> Int
hailstone n = if (mod n 2 == 0) then (div n 2) else (n*3+1)

-- Задача 8 ----------------------------------------- 
hailSeq :: Int -> [Int]
hailSeq k = k : (if (k==1) then [] else hailSeq (hailstone k))

-- Задача 9 ----------------------------------------- 
allHailSeq :: [[Int]]
allHailSeq = [hailSeq(x) | x<-[1,2..]]

-- Задача 10 ----------------------------------------- 
firstHailSeq :: Int -> Int
firstHailSeq l = head (head [x | x<-allHailSeq, (length x)==l])