{-# OPTIONS_GHC -Wall #-}
module Vanin05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------  DONE
addOne :: String -> Char -> String  
addOne st c | elem c st = st 
            | otherwise = sort (st ++ [c])

addAll :: String -> String -> String 
addAll st [] = st
addAll st (w:wd) = sort ( addAll ( addOne st w ) wd)

addWithout :: String -> String -> String 
addWithout st [] = st
addWithout st ('$':wd) = sort (addAll st wd)
addWithout st (w:wd) = sort ( addAll ( addOne st w ) wd)

inter :: String -> String -> String 
inter [] _ = []
inter _ [] = []
inter st1 st2 = sort [x | x<-st1, elem x st2]

-- Задача 2 ------------------------------------ DONE
tkPredict :: Predict -> Char -> String 
tkPredict [] _ = ""
tkPredict (p:pt) n | (fst p) == n = snd p 
                   | otherwise = tkPredict pt n
-- ex2_1_1 = [('E',"(d"), ('F',"(d"), ('T',"(d")]

upPredict :: Predict -> Char -> String -> Predict 
upPredict [] n st = [(n,st)]
upPredict (p:pt) n st | fst p == n = (n,st):pt 
                      | (n < fst p) = (n,st):p:pt 
                      | otherwise = p:(upPredict pt n st) 

-- Задача 3 ------------------------------------   DONE
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl word = res where 
    (_,_,res) = until cond stepParse (word++"$",(fst $ head gr):"$", Just []) where
        cond :: (String, String, Maybe [Int]) -> Bool
        cond (inp,stk,output) = (inp=="$") && (stk == "$") || (output == Nothing)
        stepParse :: (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
        stepParse = step gr ctl

isLower :: Char -> Bool
isLower c = not (isUpper c)

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step _ _ (input, stack, Nothing) = (input, stack, Nothing)
step gr ctl (i:input, s:stack, Just out) 
           | (isLower i) && (isLower s) && (i==s) && (i/='$') = (input,stack,Just out)
           | otherwise = makeStep stepRule where 
           stepRule :: Maybe ((Char,Char),Int)
           stepRule = find (\x -> ((fst $ fst x) == s) && ((snd $ fst x) == i)) ctl
           makeStep :: Maybe ((Char,Char),Int) -> (String, String, Maybe [Int])
           makeStep Nothing = ((i:input),(s:stack), Nothing)
           makeStep (Just (_,index)) = (i:input,(snd (gr!!index))++stack, Just (out++[index]))
step _ _ ([],[],res) = ([],[],res)
step _ _ _ = ([],[],Nothing)

-- Задача 4 ------------------------------------  DONE

first :: Predict -> String -> String
first _ [] = "$" 
first pFst (x:xs) | isUpper x = let res = tkPredict pFst x
                            in if elem '$' res then addWithout (first pFst xs) res else res 
                           | x=='$' = first pFst xs
                           | otherwise = [x]

--- ex4 = [('S',"(d"), ('V',"$+-")]


-- Задача 5 ------------------------------------  Done
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl gr pFst pNxt = sort (concatMap (\x -> makeControl x pFst pNxt) (zip [0..] gr))

makeControl :: (Int,Production) -> Predict -> Predict -> [((Char,Char),Int)]
makeControl (i,(symb, ruleRes)) predFst predNext 
            | elem '$' (first predFst ruleRes) = [ ((symb,res),i) | res <- tkPredict predNext symb]
            | otherwise =  [((symb,res),i) | res <- (first predFst ruleRes), not (isUpper res)] 


-- Задача 6 ------------------------------------  Done

testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = foldr (&&) True (map check (fromGrammar gr)) where 
    check :: (Char,[String]) -> Bool
    check (v,rls) = (testFst rls pFst) && (testFollow (tkPredict pNxt v) rls pFst)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr =  [ (v,rls) | v <- rmdups (map fst gr), let rls = buildRls gr v ]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

buildRls :: Grammar -> Char -> [String]
buildRls gr symb = [rh | (lh,rh) <- gr, lh == symb ]

testFst :: [String] -> Predict -> Bool
testFst (_:[]) _ = True
testFst rls pFst = null ( foldr1 (inter) [first pFst x | x<- rls] )

testFollow :: String -> [String] -> Predict -> Bool
testFollow fs rls pFst | elem "" rls = foldr (&&) True [ null (inter (first pFst s) fs) | s <- (delete "" rls)]
                       | otherwise = True


-- Задача 7 ------------------------------------   DONE
buildFst :: Grammar -> Predict 
buildFst gr = fst (until cond stepFst ((createFst gr), False)) where
    cond :: (Predict, Bool) -> Bool
    cond (_,isFinished) = isFinished
    stepFst :: (Predict, Bool) -> (Predict,Bool)
    stepFst (prevTable,_) = if (newTable==prevTable) then (newTable,True) else (newTable,False) 
                    where newTable = evalFst gr prevTable

createFst :: Grammar -> Predict 
createFst gr = [ (v, if canDisappear then "$" else "") | (v,rls) <- fromGrammar gr, let canDisappear = elem "" rls]

evalFst :: Grammar -> Predict -> Predict
evalFst gr pFst = evalStep gr pFst where
    evalStep::Grammar -> Predict -> Predict
    evalStep [] fstTable = fstTable
    evalStep (rule:grLeft) fstTable = evalStep grLeft (extandFst fstTable rule)

extandFst :: Predict -> Production -> Predict 
extandFst pFst (n,rul) = updateTable [(first pFst rul),(tkPredict pFst n)] where 
    updateTable :: [String] -> Predict
    updateTable sets = upPredict pFst n ( foldr1 (addAll) sets)

-- Задача 8 ------------------------------------  DONE

buildNxt :: Grammar -> Predict -> Predict 
buildNxt gr pFst = fst (until cond stepNxt ((createNxt gr),False)) where
    cond :: (Predict, Bool) -> Bool
    cond (_,isFinished) = isFinished
    stepNxt :: (Predict,Bool) -> (Predict,Bool)
    stepNxt (prevTable,_) = if (newTable==prevTable) then (newTable,True) else (newTable,False) 
                     where newTable = evalNxt (nontermTails gr) pFst prevTable

nontermTails :: Grammar -> [(Char,String)] 
nontermTails gr = [ (ch, tai) | (ch,s) <-gr, (i,symb)<-(zip [0..] s), isUpper symb, let tai = drop i s]

createNxt :: Grammar -> Predict
createNxt gr = rmdups ((startSymb,"$"):[ (l,"") | (l,_) <- gr, l/=startSymb]) where startSymb = fst (head gr)

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt tailsNxt pFst pNxt = evalStep tailsNxt pNxt where 
    evalStep :: [(Char,String)] -> Predict -> Predict
    evalStep [] t0 = t0
    evalStep ((n,ta):tailLeft) t0 = evalStep tailLeft (extandNxtOne pFst n t0 ta)

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne _ _ _ [] = error "Error"
extandNxtOne pFst n pNxt (m:st) 
             | elem '$' (first pFst st) = updateTable [(tkPredict pNxt m),(delete '$' (first pFst st)),(tkPredict pNxt n)]
             | otherwise = updateTable [(tkPredict pNxt m),(first pFst st)] where
             updateTable :: [String] -> Predict
             updateTable sets = upPredict pNxt m (foldr1 (addAll) sets)

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

