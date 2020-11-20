{-# OPTIONS_GHC -Wall #-}
module Vanin07 where
import Data.List(sort)

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-äåðåâî ïîðÿäêà t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ãîëîâí³ õàðàêòåðèñòèêè B-äåðåâî  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Çàäà÷à 1 ------------------------------------  DONE
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k tl tr) | k<=0 = False
                           | otherwise = (checkValid v tl tr) && (isSearch tl) && (isSearch tr)

checkValid :: (Ord a) => a -> BinTreeM a -> BinTreeM a -> Bool
checkValid _ (EmptyM) (EmptyM) = True
checkValid currV (EmptyM) (NodeM vR _ _ _) = currV < vR
checkValid currV (NodeM vL _ _ _) (EmptyM) = currV > vL
checkValid currV (NodeM vL _ _ _) (NodeM vR _ _ _) = currV > vL && currV < vR

-- Çàäà÷à 2 ------------------------------------  DONE
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ tl tr) toFind | v == toFind = True
                                    | toFind < v = elemSearch tl toFind
                                    | otherwise = elemSearch tr toFind

-- Çàäà÷à 3 ------------------------------------  DONE
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM v k tl tr) toInsert | v == toInsert = (NodeM v (k+1) tl tr)
                                     | toInsert < v = NodeM v k (insSearch tl toInsert) tr
                                     | otherwise = NodeM v k tl (insSearch tr toInsert)

-- Çàäà÷à 4 ------------------------------------   DONE   ???
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch (EmptyM) _ = EmptyM
delSearch (NodeM v k tl tr) toDel 
 | v==toDel && k > 1 = NodeM v (k-1) tl tr
 | toDel > v = NodeM v k tl (delSearch tr toDel)
 | toDel < v = NodeM v k (delSearch tl toDel) tr
 | otherwise = case (tl,tr) of
               (EmptyM, EmptyM) -> EmptyM
               (EmptyM, tr1) -> tr1
               (tl1, EmptyM) -> tl1
               _ -> case findLeftBigger tr of 
                     Just (a,b) -> NodeM a b tl (delSearch tr a)
                     Nothing -> tr

findLeftBigger:: (Ord a) => BinTreeM a -> Maybe (a,Int)
findLeftBigger (EmptyM) = Nothing
findLeftBigger (NodeM v k (EmptyM) _) =  Just (v,k) 
findLeftBigger (NodeM _ _ tl _) = findLeftBigger tl

-- delSearch (NodeM  't' 1 EmptyM EmptyM) 't'
-- delSearch (NodeM  't' 2 EmptyM EmptyM) 't' 

-- Çàäà÷à 5 ------------------------------------  DONE
sortList :: (Ord a) => [a] -> [a]
sortList xs = binTreeToList $ listToBinTree xs

listToBinTree :: (Ord a) => [a] -> BinTreeM a
listToBinTree xs = foldl insSearch EmptyM xs

binTreeToList :: (Ord a) => BinTreeM a -> [a]
binTreeToList EmptyM = []
binTreeToList (NodeM v k tl tr) = (binTreeToList tl) ++ (nodeToList v k) ++ (binTreeToList tr)

nodeToList :: (Ord a) => a -> Int -> [a]
nodeToList _ 0 = []
nodeToList v k = v : (nodeToList v (k-1))

-- Çàäà÷à 6 ------------------------------------  DONE
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform t = BInform (findHeight t) (findMin t) (findMax t)

findHeight :: (Bounded a, Ord a) => Btree a ->  Int
findHeight (NodeB _ []) = 0
findHeight (NodeB _ ts) = 1 + (findHeight $ head ts)

findMin :: (Bounded a, Ord a) => Btree a ->  a
findMin (NodeB (v:_) []) = v
findMin (NodeB _ subTs) = findMin $ head subTs

findMax :: (Bounded a, Ord a) => Btree a ->  a
findMax (NodeB vals []) = last vals
findMax (NodeB _ subTs) = findMax $ last subTs

-- Çàäà÷à 7 ------------------------------------   DONE
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree t tr@(NodeB _ ts) = (checkRoot t tr) && (checkSorted tr) && (and $ map (checkLength t) ts) && (checkBalance tr)

checkRoot :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkRoot t (NodeB vals subTs) = n >= 1 && n <= (2*t-1) && (length subTs) == (n+1)
                  where n = length vals

checkLength :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkLength t (NodeB vals subTs) = (n <= (2*t-1)) && (n >= (t-1)) && (and $ map (checkLength t) subTs)
                  where n = length vals

checkBalance :: (Bounded a, Ord a) => Btree a -> Bool            
checkBalance (NodeB vals subTs) = (compareValsToSubTs vals subTs) && (and $ map checkBalance subTs) where
      compareValsToSubTs [] _ = True
      compareValsToSubTs _ [] = True
      compareValsToSubTs (v:otherVals) ((NodeB lowerVals _):otherTs) = (and $ map (<=v) lowerVals) && (compareValsToSubTs otherVals otherTs)

checkSorted :: (Bounded a, Ord a) => Btree a -> Bool
checkSorted (NodeB vals subTs) = isSorted vals && (and $ map checkSorted subTs)
              
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- isBtree 3 (NodeB "b" [NodeB "aac" [], NodeB "bfr" []])
-- isBtree 3 tBt6

-- Çàäà÷à 8 ------------------------------------  DONE
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = (sort $ toList tr1) == (sort $ toList tr2)

toList :: Btree a -> [a]
toList (NodeB vals subTs) = vals ++ (concatMap toList subTs)

-- Çàäà÷à 9 ------------------------------------   DONE
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr v = elem v (toList tr)

position :: Ord a => a -> [a] -> Int
position v xs = case [i | (i, x) <- zip [0..] xs, v <=x] of
    [] -> 0
    (x:_) -> x

-- Çàäà÷à 10 ------------------------------------   DONE
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t root v | (isFull t root) = 
                    let (tl, vals, tr) = splitAtB t root
                    in insertIntoNode t (NodeB [vals] [tl,tr]) v
                  | otherwise = insertIntoNode t root v

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB vals _) = length vals == 2*t-1

insertKey :: Ord a => a -> [a] -> [a]
insertKey v (x:xs) | v <= x = v:x:xs
                   | otherwise = x:(insertKey v xs)
insertKey v [] = [v]

insertIntoNode :: Ord a => Int -> Btree a -> a -> Btree a
insertIntoNode _ (NodeB vals []) v = NodeB (insertKey v vals) []
insertIntoNode t (NodeB vals subTs) v = let (valsL,valsR,subTsL,midTree,subTsR) = decomposeNodeB v vals subTs
    in if (isFull t midTree) then 
          let (midTreeL,k,midTreeR) = splitAtB t midTree
              newMidTreeL = if v < k then (insertIntoNode t midTreeL v) else midTreeL
              newMidTreeR = if v < k then midTreeR else (insertIntoNode t midTreeR v)
          in NodeB (valsL ++ [k] ++ valsR) (subTsL ++ [newMidTreeL,newMidTreeR] ++ subTsR)
       else NodeB vals (subTsL ++ [insertIntoNode t midTree v] ++ subTsR)


decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v vals subTs = (valsL,valsR,subTsL,midTree,subTsR) where
        pos = position v vals
        subTsL = take pos subTs         
        subTsR = drop (pos+1) subTs     
        valsL  = take pos vals          
        valsR  = drop pos vals          
        midTree = subTs!!pos

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB vals subTs)= let midVal = vals !! ((+) (div t 2) 1) 
                                   (valsL, valsR) = splitAt ((+) (div t 2) 1) vals
                                   (subTsL, subTsR) = splitAt ((+) (div t 2) 2) subTs
                               in ((NodeB valsL subTsL), midVal, (NodeB (tail valsR) subTsR))
c1,c2,c3,c4,res ::Bool
c1 = insBtree 3 tBt5 'B' == tBt6
c2 = insBtree 3 tBt6 'Q' == tBt7
c3 = insBtree 3 tBt7 'L' == tBt8
c4 = insBtree 3 tBt8 'F' == tBt9
res = and [c1,c2,c3,c4]

---------------------Òåñòîâ³ äàí³ - Äåðåâà ïîøóêó -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
