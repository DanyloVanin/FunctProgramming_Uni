{-# OPTIONS_GHC -Wall #-}
module Vanin03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (b:bs) (x:xs) | b==x = isPrefix bs xs
                       | otherwise = False

-- Задача 2 ------------------------------------
splitStr :: String -> Int -> (String,String)
splitStr str pos = (take pos str, drop pos str)

substitute :: Substitution -> Int -> String -> String
substitute (from,to,_) i w = let (lhs,rhs) = splitStr w i
                                 rightPart = if (isPrefix from rhs) 
                                                then (to++(drop (length from) rhs)) 
                                                else rhs
                             in lhs++rightPart

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w sub = [(sub,x) | x <- [0..(length w)],
                                let (_,rhs) = splitStr w x,
                                let (from,_,_) = sub, (isPrefix from rhs)]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = [x | rule<-algo, x<-(findPosition w rule)]

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA _ (False,st,word) = (False,st,word)
stepA algo (_,st,word) = 
                let ((from,to,state),i) = head (findAll algo word)
                in (not state,st+1,substitute (from,to,state) i word)

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = 
    let (canContinue, totalSteps, finalWord) = until cond step (True, 0, word)
                            where step :: ConfigA -> ConfigA
                                  step = stepA algo
                                  cond :: ConfigA -> Bool
                                  cond (st, stepNum, _) = (stepNum >= m || (not st) )
    in if ((totalSteps >= m) && canContinue ) then Nothing else Just finalWord
-- Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg [] = 0
maximReg (r:rs) = max (getMaxReg r 0) (maximReg rs)
    where getMaxReg :: Command -> Int -> Int
          getMaxReg (Z r1) = max r1 
          getMaxReg (S r1) = max r1 
          getMaxReg (T r1 r2) = max (max r1 r2) 
          getMaxReg (J r1 r2 _) = max (max r1 r2)

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir = ir ++ [0 | _<-[1..((maximReg pr)-(length ir))]]

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v = (take r reg)++[v]++(drop (r+1) reg) 

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm, st, reg) =
    let useCommand :: Command -> ConfigC -> ConfigC
        useCommand (Z r1) (comNum, steps, regs) = (comNum+1, steps+1, upd regs (r1-1) 0)
        useCommand (S r1) (comNum, steps, regs) = (comNum+1, steps+1, upd regs (r1-1) ((regs!!(r1-1))+1))
        useCommand (T r1 r2) (comNum, steps, regs) = (comNum+1, steps+1, upd regs (r2-1) (reg!!(r1-1)))
        useCommand (J r1 r2 nxtCom) (comNum, steps, regs) = if (regs!!(r1-1)) == (regs!!(r2-1)) then (nxtCom, steps+1, regs) else (comNum+1,steps+1,regs)
    in useCommand (pr!!(nm-1)) (nm, st, reg)

-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir = 
    let (comNum, totalSteps, regs) = until cond step (1,0,ini pr ir)
                            where step :: ConfigC -> ConfigC
                                  step = stepC pr
                                  cond :: ConfigC -> Bool
                                  cond (nxtCom, stepNum, _) = (stepNum >= mx || (nxtCom>(length pr)) )
    in if ((totalSteps >= mx) && (comNum<=(length pr))) then Nothing else Just (regs!!0)

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
