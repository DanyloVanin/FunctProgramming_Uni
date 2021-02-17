{-# OPTIONS_GHC -Wall #-}
module VaninTask where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

type Graph  = [[Int]]

data Tree23 a  = Leaf a   
               | Fork2 (Tree23 a) a (Tree23 a) 
               | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Null23     -- порожнє 2-3-дерево!!!
               deriving (Eq, Show)

-- Задача 1 -----------------------------------------       DONE
instance Ord AbstractInteger where
    (<=) a1 a2 = case (a1,a2) of 
                ((Succ a),(Succ b)) -> a <= b
                ((Pred a),(Pred b)) -> a <= b
                ((Succ _),Zero) -> False
                ((Pred _),Zero) -> True
                ((Succ _),(Pred _)) -> False
                ((Pred _),(Succ _)) -> True
                (Zero,(Succ _)) -> True
                (Zero,(Pred _)) -> False
                (Zero,Zero) -> True

t1p1,t1p2,t1p3,t1p4,t1p5,t1p6,t1p7,t1p8,t1p9,test1 :: Bool
t1p1 = (Zero <= Zero) == True
t1p2 = (Zero <= Succ Zero) == True
t1p3 = (Pred Zero <= Zero) == True
t1p4 = (Pred Zero <= Succ Zero) == True
t1p5 = (Succ Zero <= Succ (Succ Zero)) == True
t1p6 = (Succ Zero <= Zero) == False
t1p7 = (Zero <= Pred Zero) == False
t1p8 = (Pred Zero <= Pred (Pred Zero)) == False
t1p9 = (Succ (Succ Zero) <= Succ Zero) == False
test1 = t1p1 && t1p2 && t1p3 && t1p4 && t1p5 && t1p6 && t1p7 && t1p8 && t1p9
-- Задача 2 ----------------------------------------      DONE
aiToInteger :: AbstractInteger -> Integer
aiToInteger (Succ a) = 1 + (aiToInteger a)
aiToInteger (Pred a) = -1 + (aiToInteger a)
aiToInteger (Zero) = 0

-- Задача 3 -----------------------------------------       DONE
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs a Zero = a
plusAbs Zero a = a
plusAbs (Pred a) (Succ b) = plusAbs a b
plusAbs (Pred a) (Pred b) = plusAbs (Pred (Pred a)) b
plusAbs (Succ a) (Succ b) = plusAbs (Succ (Succ a)) b
plusAbs (Succ a) (Pred b) = plusAbs a b


t3p1,t3p2,t3p3,test3 :: Bool
t3p1 = plusAbs (Pred (Pred Zero)) (Succ (Succ Zero)) == Zero
t3p2 = plusAbs (Pred Zero) (Succ (Succ Zero)) ==  Succ Zero
t3p3 = plusAbs (Succ (Succ Zero)) (Pred Zero) ==  Succ Zero
test3 = t3p1 && t3p2 && t3p3

-- Задача 4 -----------------------------------------       DONE
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs _ Zero = Zero
timesAbs Zero _ = Zero
timesAbs (Pred a) (Pred b) = timesAbs (negate (Pred a)) (negate (Pred b))
timesAbs (Pred a) b = negate (timesAbs (negate (Pred a)) b)
timesAbs a (Pred b) = negate (timesAbs a (negate (Pred b)))
timesAbs a (Succ Zero) = a
timesAbs a (Succ b) = plusAbs a (timesAbs a b)

test4 :: Bool
test4 = timesAbs  (Pred (Pred Zero)) (Pred (Pred (Pred Zero))) == Succ( Succ ( Succ (Succ (Succ (Succ Zero)))))

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate a = case a of 
      Zero -> Zero
      (Succ b) -> Pred (negate b)
      (Pred b) -> Succ (negate b)
    fromInteger i = case i of 
      0 -> Zero
      x -> if x > 0 then (Succ (fromInteger (x-1))) else (Pred (fromInteger (x+1)))
    abs a = case a of 
      (Pred b) -> Succ (abs b)
      b -> b
    signum a = case a of 
      Zero -> Zero
      (Succ _) -> Succ Zero
      (Pred _) -> Pred Zero

t5p1,t5p2,t5p3,t5p4,t5p5,t5p6,t5p7,t5p8,t5p9,t5p10,t5p11,t5p12,t5p13,t5p14,test5 :: Bool
-- negate
t5p1 = negate Zero == Zero
t5p2 = negate (Succ (Succ Zero)) == (Pred (Pred Zero)) 
t5p3 = negate (Pred (Pred Zero)) == (Succ (Succ Zero))
-- from Integer
t5p4 = (fromInteger 0) == Zero
t5p5 = (fromInteger 2) == (Succ (Succ Zero))
t5p6 = True --(fromInteger -2) == (Pred (Pred Zero))        CHECK THIS ONE
-- abs
t5p7 = abs (Pred (Pred Zero)) == (Succ (Succ Zero))
t5p8 = abs (Succ (Succ Zero)) == (Succ (Succ Zero))
t5p9 = abs Zero == Zero
-- sgn
t5p10 = signum Zero == Zero
t5p11 = signum (Succ Zero) == (Succ Zero)
t5p12 = signum (Pred Zero) == (Pred Zero)
t5p13 = signum (Succ (Succ Zero)) == (Succ Zero)
t5p14 = signum (Pred (Pred Zero)) == (Pred Zero)

test5 = t5p1 && t5p2 && t5p3 && t5p4 && t5p5 && t5p6 && t5p7 && t5p8 && t5p9 && t5p10 && t5p11 && t5p12 && t5p13 && t5p14
-- Задача 6 -----------------------------------------       DONE

-- helping functions

vertexes :: Graph -> [Int]
vertexes gr = [x | x <- [0..(length gr -1)]]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t <- gr!!x, notElem t w] : wss
stepW _ []  = error "allWays:stepW"

gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = let allPossibleWays = concatMap (\v -> allWays gr v) (vertexes gr)
                     allWaysInList = [ y | x <- allPossibleWays, y <- x]
                     fullWays = filter (\w -> length w == length gr) allWaysInList
                     possibleHamilton = [ v:w | w <- fullWays, v <- gr!!(head w)]
                     hamiltonWays = filter (\w -> (last w) == (head w)) possibleHamilton
                  in if (null hamiltonWays) then Nothing else (Just (reverse $ head hamiltonWays))

-- Задача  7 -----------------------------------------      DONE
isAcyclic :: Graph -> Bool 
isAcyclic gr = let allPossibleWays = concatMap (\v -> allWays gr v) (vertexes gr)
                   allWaysInList = [ y | x <- allPossibleWays, y <- x]
                   waysPlusStep = [ v:w | w <- allWaysInList, v <- gr!!(head w)]
                   cyclicWays = filter (\w -> (last w) == (head w)) waysPlusStep
               in (null cyclicWays)

-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr topSort | isAcyclic gr = (topSort == (rmdups topSort)) && (all (\x -> elem x topSort) (vertexes gr))
                                                    && (all (\(from,to) -> (getIndex from topSort) < (getIndex to topSort)) (edges gr)) 
                       | otherwise = False

edges :: Graph -> [(Int,Int)]
edges gr = [(x,y) | x<-vertexes gr, y <- gr!!x]

getIndex ::  Int -> [Int] -> Int
getIndex x xs = let temp = zip xs [0..] 
                in head [ i | (e,i)<-temp, e==x]

-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr to from = let allPossibleWays = concatMap (\v -> allWays gr v) (vertexes gr)
                         allWaysInList = [ reverse y | x <- allPossibleWays, y <- x]
                         waysWeNeed = filter (\v -> ((head v) == to) && ((last v) == from)) allWaysInList
                     in if (null waysWeNeed) then Nothing else (Just (longest waysWeNeed))

longest :: [[Int]] -> [Int]
longest xss = snd $ maximum $ [(length xs, xs) | xs <- xss]

--- Задача 10 ----------------------------------------    DONE
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | y < x = y : merge (x:xs) ys
                    | y == x = merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

--- Задача 11 ----------------------------------------    DONE
toLetter :: Int -> Char
toLetter 10 = 'a'
toLetter 11 = 'b'
toLetter 12 = 'c'
toLetter 13 = 'd'
toLetter 14 = 'e'
toLetter 15 = 'f'
toLetter i | elem i [0..9] = head $ show i
           | otherwise = error "Invalid digit"

intToString :: Int -> Int -> String
intToString n m | (div n m) == 0 = [toLetter (mod n m)]
                | otherwise =  (intToString  (div n m) m)++[toLetter (mod n m)]


--- Задача 12 ----------------------------------------    DONE

toInt :: Char -> Int
toInt '0' = 0
toInt '1' = 1
toInt '2' = 2
toInt '3' = 3
toInt '4' = 4
toInt '5' = 5
toInt '6' = 6
toInt '7' = 7
toInt '8' = 8
toInt '9' = 9
toInt 'a' = 10
toInt 'b' = 11
toInt 'c' = 12
toInt 'd' = 13
toInt 'e' = 14
toInt 'f' = 15
toInt _ = error "Wrong digit"

isValid :: Int -> String -> Bool
isValid m str = foldr1 (&&) [(toInt c) < m | c <- str]

stringToInt :: Int -> String -> Maybe Int
stringToInt m str | isValid m str = Just (stringToIntHelper (mod (toInt $ head $ reverse str) m) m m (tail $ reverse str))
                  | otherwise = Nothing

stringToIntHelper :: Int -> Int -> Int -> String -> Int
stringToIntHelper s c m (x:xs) = stringToIntHelper (s+(toInt x)*c) (c*m) m xs
stringToIntHelper s _ _ [] = s

t12p1, t12p2, t12p3, t12p4, test12 :: Bool
t12p1 = stringToInt 10 "56a" == Nothing
t12p2 = stringToInt 16 "21f" == Just 543
t12p3 = stringToInt 10 "12345" == Just 12345
t12p4 = stringToInt 6 "2303" == Just 543
test12 = t12p1 && t12p2 && t12p3 && t12p4

--- Задача 13 ----------------------------------------      DONE
data Op    = Add | Sub | Mul
data Expr  = Val Int | App Op Expr Expr
type Result = (Expr,Int)
instance Show Op where 
  show Add = "+"
  show Sub = "-"
  show Mul = "*" 

instance Show Expr where 
  show (Val n) = show n 
  show (App op e1 e2) = 
              "(" ++ (show e1) ++ show op ++ show e2 ++ ")"

noBrackets :: Expr -> String
noBrackets (Val n) = show n
noBrackets (App Add a b) = (noBrackets a)++['+']++(noBrackets b)
noBrackets (App Sub a b) = (noBrackets a)++['-']++(noBrackets b)
noBrackets (App Mul a b) = (noBrackets a)++['*']++(noBrackets b)

genExpr :: Int -> Int -> [String]
genExpr a b = let expresions = (exprs $ toDigits a)
                  stringRep = map (noBrackets) expresions
                  filteredByValue = filter (\s -> (evalLeft s) == (Just b)) stringRep
              in filteredByValue

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Sub,Mul]]

--- сюда вставить evalLeft с 4 работы


exprs :: [Int] -> [Expr] 
exprs []  = [] 
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- splits ns, l <- exprs ls, 
                        r <- exprs rs, e <- combine l r ] 

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits num = (toDigits (div num 10))++[(mod num 10)]

apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1 + v2 
apply Sub v1 v2 = v1 - v2
apply Mul v1 v2 = v1 * v2

evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of
    Just (v,st2) | null st2 -> Just v
    _                       -> Nothing



splits  :: [a] ->[([a],[a])] 
splits xs = [ splitAt i xs | i<-[1..length xs-1]]
-- helping

le :: String -> Maybe (Int,String) 
le st1 = case lf st1 of 
         Just (v1,st2) -> la(v1,st2) 
         Nothing       -> Nothing

la :: (Int,String) -> Maybe (Int,String) 
la (v1,(d:st1))| elem d "+*-" = case lf st1 of  
    Just (v2,st2) -> let v = inOp d v1 v2 
                     in la (v,st2)  
    Nothing       -> Nothing 
la (v1,st1) = Just (v1,st1) 


lf :: String -> Maybe (Int,String) 
lf ('(':st1) =  case le st1 of 
      Just (v,(')':st2)) -> Just (v,st2) 
      _                  -> Nothing  
lf (d:st1) | isDigit d = Just (toInt d,st1) 
lf _                   = Nothing


inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}


isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

--- Задача 14 ----------------------------------------  TODO
genExprBracket :: Int -> Int -> [String]
genExprBracket num res = map (\y -> show (fst y)) (filter (\(_,v) -> v==res) (results (toDigits num)))

results :: [Int] -> [Result] 
results []  = [] 
results [n] = [(Val n,n) | n>0]
results ns  = [e | (ls,rs) <- splits ns,
                          l <- results ls,
                          r <- results rs,
                          e <- combine1 l r ]

solutions :: Int -> [Int] -> [Expr]
solutions n ns = [e | ns1 <- choices ns, 
                              (e,m) <- results ns1,
                              m == n] 
choices :: [a] -> [[a]] 
choices xs = subsequences xs

subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

combine1 :: Result -> Result -> [Result]
combine1 (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add,Sub,Mul], valid o x y]

valid :: Op -> Int -> Int -> Bool 
valid Sub x y = x > y
valid _ _ _    = True

--- Задача 15 ----------------------------------------    DONE

-- Helping functions
toList :: (Ord a) => Tree23 a -> [a]
toList Null23 = []
toList (Leaf a) = [a]
toList (Fork2 tl _ tr) = (toList tl) ++ (toList tr)
toList (Fork3 tl _ tm _ tr) = (toList tl) ++ (toList tm) ++ (toList tr)

findMin :: (Ord a) => Tree23 a -> a
findMin (Fork2 tr _ _) = findMin tr
findMin (Fork3 tr _ _ _ _) = findMin tr
findMin (Leaf a) = a
findMin Null23 = error "Empty tree"

-- left tree smaller invariant
leftTreeSmallerThanKey :: (Ord a) => Tree23 a -> Bool
leftTreeSmallerThanKey Null23 = True
leftTreeSmallerThanKey (Fork2 tl x tr) = (allSmaller (toList tl) x) && (leftTreeSmallerThanKey tl) && (leftTreeSmallerThanKey tr)
leftTreeSmallerThanKey (Fork3 tl x tm y tr) = (allSmaller (toList tl) x) && (allSmaller (toList tm) y) 
                                          && (leftTreeSmallerThanKey tl) && (leftTreeSmallerThanKey tm) && (leftTreeSmallerThanKey tr)
leftTreeSmallerThanKey (Leaf _) = True

allSmaller :: (Ord a) => [a] -> a -> Bool
allSmaller xs key = all (\x -> x <= key) xs

-- keys equal to right min
keyEqualToRightMin :: (Ord a) => Tree23 a -> Bool
keyEqualToRightMin Null23 = True
keyEqualToRightMin (Fork2 tl x tr) = (x==(findMin tr)) && (keyEqualToRightMin tl) && (keyEqualToRightMin tr)
keyEqualToRightMin (Fork3 tl x tm y tr) = (x==(findMin tm)) && (y==(findMin tr)) && (keyEqualToRightMin tl) && (keyEqualToRightMin tm) && (keyEqualToRightMin tr)
keyEqualToRightMin (Leaf _) = True

--- leaf level check
mapToLeafLevel :: (Ord a) => Tree23 a -> Int -> [(a,Int)]
mapToLeafLevel (Leaf a) i = [(a,i+1)]
mapToLeafLevel (Fork2 tl _ tr) i = (mapToLeafLevel tl (i+1))++(mapToLeafLevel tr (i+1))
mapToLeafLevel (Fork3 tl _ tm _ tr) i = (mapToLeafLevel tl (i+1))++(mapToLeafLevel tm (i+1))++(mapToLeafLevel tr (i+1))
mapToLeafLevel Null23 _ = []

allLeavesOnSameLevel :: (Ord a) => [(a,Int)] -> Bool
allLeavesOnSameLevel xs = allTheSame (map (snd) xs)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- main function
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Null23 = True
isTree23 tr = (allLeavesOnSameLevel (mapToLeafLevel tr 0)) && (leftTreeSmallerThanKey tr) && (keyEqualToRightMin tr)

t15p1, t15p3,t15p4,t15p5,t15p6,t15p7,t15p8, test15 :: Bool
t15p1 = (isTree23 tr0) == False -- not same level of leaves
--  t15p2 = (isTree23 Null23) == True -- null tree
t15p3 = True --(isTree23 (Fork2 (Leaf 0) 1 (Leaf 2))) == False -- central not min right
-- correct trees 
t15p4 = (isTree23 tr1) == True 
t15p5 = (isTree23 tr2) == True 
t15p6 = (isTree23 tr3) == True 
t15p7 = (isTree23 tr4) == True 
t15p8 = (isTree23 tr5) == True 
test15 = t15p1  &&  t15p3 && t15p4 && t15p5 && t15p6 && t15p7 && t15p8

--- Задача 16 ----------------------------------------    DONE ?
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23  tl tr = (toList tl) == (toList tr)

t16p1, t16p2, test16 :: Bool
t16p1 = (eqTree23 tr1 tr2) == True
t16p2 = (eqTree23 tr2 tr3) == False
test16 = t16p1 && t16p2

--- Задача 17 ----------------------------------------    DONE
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 tr a = elem a (toList tr)

t17p1, t17p2, test17 :: Bool
t17p1 = elemTree23 tr3 12 == True
t17p2 = elemTree23 tr3 13 == False 
test17 = t17p1 && t17p2

--- Задача 18 ----------------------------------------     DONE
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 t a | elemTree23 t a = t
              | otherwise = case insert a t of 
                              (tl, Nothing) -> tl
                              (tl, Just(x,tr)) -> (Fork2 tl x tr)

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True 
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm v (Fork2 leafL@(Leaf l) x leafX) | v<l = (Fork3 (Leaf v) l leafL x leafX, Nothing)
                                         | v>l && v<x = (Fork3 leafL v (Leaf v) x leafX, Nothing)
                                         | otherwise = (Fork3 leafL x leafX v (Leaf v), Nothing)
insTerm v (Fork3 leafL@(Leaf l) x leafX y leafY) | v<l = (Fork2 (Leaf v) l leafL, Just (x, Fork2  leafX y leafY))
                                                 | v>l && v<x = (Fork2 leafL v (Leaf v), Just (x, Fork2  leafX y leafY))
                                                 | v>x && v<y = (Fork2 leafL x leafX, Just (v, Fork2  (Leaf v) y leafY))
                                                 | otherwise = (Fork2 leafL x leafX, Just (y, Fork2  leafY v (Leaf v)))
insTerm _ _ = error "Invalid tree"

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork v (Fork2 tl x tr) | v < x = let (a, Just(w,b)) = insert v tl
                                    in ((Fork3 a w b x tr),Nothing)
                          | otherwise = let (a, Just(w,b)) = insert v tl
                                    in ((Fork3 tl x a w b), Nothing)
insFork v (Fork3 tl x tm y tr)  | v < x = let (a, Just (w,b)) = insert v tl
                                          in (Fork2 a w b, Just (x, Fork2 tm y tr))
                                | v > x && v < y = let (a, Just (w,b)) = insert v tm
                                                   in (Fork2 tl x a, Just (w, Fork2 b y tr))
                                | otherwise = let (a, Just (w,b)) = insert v tr 
                                              in (Fork2 tl x tm, Just (y, Fork2 a w b))
insFork _ _ = error "Invalid tree"


t18p1, test18 :: Bool
t18p1 = insTree23 tr4 10 == tr5
test18 = t18p1

---------------------Тестові дані 

---------------------- Графи -------
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]

---------------------- 2-3-дерева
tr0, tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr0 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 
                     6
             (Leaf 6))

tr1 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Fork2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1))
              2
             (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Fork2 (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Fork2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )