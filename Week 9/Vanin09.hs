{-# OPTIONS_GHC -Wall #-}
module Vanin09 where

import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------   DONE
simplify :: RE -> RE   
simplify Null = Null
simplify (Term char) = Term char
simplify (Seq first second) = Seq (simplify first) (simplify second)
simplify (Alt first second) = Alt (simplify first) (simplify second)
simplify (Rep r) = Rep (simplify r)
simplify (Plus r) = Seq (simplify r) (Rep (simplify r))
simplify (Opt r) = Alt (simplify r) Null

-- Задача 2 -----------------------------------------   DONE
isTerminal :: Automation -> State -> Bool 
isTerminal (_,terminals,_) s = elem s terminals

isEssential :: Automation -> State -> Bool 
isEssential aut s = (isTerminal aut s) || (canRead aut s)

canRead :: Automation -> State -> Bool
canRead (_,_,trans) s = (not $ null $ filter (\(from,_,l) -> (from==s) && (isChar l)) trans)

isChar :: Label -> Bool
isChar l = case l of 
  C _ -> True
  _-> False

-- Задача 3 -----------------------------------------   DONE
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, trans) s = [ x | x@(from, _, _) <- trans, from == s]

-- Задача 4 -----------------------------------------    DONE
labels :: [Transition] -> [Label]
labels trans = nub [ l | (_,_,l) <- trans, isChar l]

-- Задача 5 -----------------------------------------     DONE
acceptsDA :: Automation -> String -> Bool
acceptsDA daut@(inSt,_,_) st = let (finState, finStr, isInterrupted) = until condDA (stepDA daut) (inSt, st, False)
                    in (isTerminal daut finState) && (null finStr) && (not isInterrupted)

condDA :: (State, String, Bool) -> Bool
condDA (_, s , isInterrupted) = isInterrupted || (null s) 

stepDA :: Automation -> (State, String, Bool) -> (State, String, Bool)
stepDA daut (st, s, _) = case findState (transitionsFrom daut st) (head s) of 
  Just newState -> (newState,tail s, False) 
  Nothing -> (st,s, True)

findState :: [Transition] -> Char -> Maybe State 
findState ((_,st,(C l)):ts) c | c == l = Just st
                              | otherwise = findState ts c
findState _ _ = Nothing

-- Задача 6 -----------------------------------------     DONE
stStep  :: Automation -> State -> Label -> [State]
stStep naut st mc = [ nxtState | (_,nxtState,l) <- (transitionsFrom naut st), l == mc]

setStep :: Automation -> [State] -> Label -> [State]
setStep naut st mc = nub $ concatMap (\x -> stStep naut x mc) st

closure :: Automation -> [State] -> [State]
closure naut st  = nub $ sort $ snd (until (\(x,_) -> null x) (closureStep naut) (st,[]))

closureStep :: Automation -> ([State],[State]) -> ([State],[State])
closureStep aut (startStates, visited) = let nxtStates = filter (\st -> notElem st visited) (setStep aut startStates Eps)
                            in (nxtStates, visited++nxtStates)

-- Задача 7 -----------------------------------------    DONE
accepts :: Automation -> String -> Bool
accepts aut@(start,_,_) st = autStep aut [start] st
     
autStep :: Automation -> [State] -> String -> Bool
autStep _ [] _ = False
autStep aut posStates [] = any (isTerminal aut) posStates
autStep aut posStates (c:sst) = autStep aut (setStep aut (closure aut posStates) (C c)) sst

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null st fin nxt = ([(st,fin,Eps)],nxt)
make (Term c) st fin nxt = ([(st,fin,C c)],nxt)
make (Seq l r) st fin nxt = (trans1++trans2++[(nxt,nxt+1,Eps)],st2) where 
  (trans1,st1) = make l st nxt (nxt+2)
  (trans2,st2) = make r (nxt+1) fin st1
make (Alt l r) st fin nxt = (trans1++trans2++trAll,st2) where 
  (trans1,st1) = make l nxt (nxt+1) (nxt+4)
  (trans2,st2) = make r (nxt+2) (nxt+3) st1
  tr3 = (st,nxt,Eps)
  tr4 = (st,nxt+2,Eps)
  tr5 = (nxt+1,fin,Eps)
  tr6 = (nxt+3,fin,Eps)
  trAll = tr3:tr4:tr5:[tr6]
make (Rep r) st fin nxt = (trans1++trAll,st1) where 
  (trans1,st1) = make r nxt (nxt+1) (nxt+2)
  tr2 = (st,fin,Eps)
  tr3 = (st,nxt,Eps)
  tr4 = (nxt+1,fin,Eps)
  tr5 = (nxt+1,nxt,Eps)
  trAll = tr2:tr3:tr4:[tr5]
make (Plus _) _ _ _ = undefined
make (Opt _) _ _ _ = undefined

-- Задача 9 -----------------------------------------     DONE
parseReg :: String -> Maybe RE 
parseReg st = case P.parse reg "" (filter (\x -> (notElem x "\t\n ")) st) of
                    Left _ -> Nothing
                    Right s -> Just s

reg :: P.Parser RE
reg = do r <- rexpr
         P.eof
         return r;

rexpr :: P.Parser RE
rexpr = do s <- rterm
           s2 <- P.many (rexprNxt)
           if null s2 then return s else return (Alt s (altBuild s2))

rexprNxt :: P.Parser RE
rexprNxt = do _ <- P.char '|' 
              r <- rterm
              return r

rterm :: P.Parser RE
rterm = do r1 <- P.many1 rfact
           return (seqBuild r1)

rfact :: P.Parser RE
rfact = do p <- prime
           op <- P.many (P.oneOf "*?+")
           return (opBuild op p)

prime :: P.Parser RE
prime = (P.try rsymb) P.<|> prime'

prime' :: P.Parser RE
prime' = do _ <- P.char '('
            r <- rexpr
            _ <- P.char ')'
            return r

rsymb :: P.Parser RE
rsymb = do c <- P.noneOf "()|*+?"
           return (Term c) 

seqBuild :: [RE] -> RE
seqBuild [r] = r
seqBuild (r:rs) = Seq r (seqBuild rs)
seqBuild [] = error "Parsing error"

altBuild :: [RE] -> RE
altBuild [r] = r
altBuild (r:rs) = Alt r (altBuild rs)
altBuild [] = error "Parsing error"

opBuild :: String -> RE -> RE
opBuild (x:xs) re = case x of
     '*' -> opBuild xs (Rep re)
     '?' -> opBuild xs (Opt re)
     '+' -> opBuild xs (Plus re)
     _  -> Null
opBuild [] re = re


-- Задача 10 -----------------------------------------  DONE
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(inState,_,_) = let (gmsx,bmsx,mlx)= until condMeta (stepMeta aut) (initMeta aut inState)
 in (head gmsx, bmsx, nub mlx)

initMeta :: Automation -> State -> ([MetaState], [MetaState], [MetaTransition])
initMeta aut inState = ([], [filter (isEssential aut) ([inState] ++ closure aut [inState])], []) 

condMeta:: ([MetaState], [MetaState], [MetaTransition]) ->Bool
condMeta (_,bmsx,_) = null bmsx

stepMeta :: Automation -> ([MetaState], [MetaState], [MetaTransition])->([MetaState], [MetaState], [MetaTransition])
stepMeta aut (gmsx,bmsx,mtrx) = let msx = head bmsx 
                                    elems = labels $ concatMap (transitionsFrom aut) msx
                                    allNewStates = map (\st -> filter (isEssential aut) (nub(st++(closure aut st)))) (map (setStep aut msx) elems)
                                    newTransitions = map (\(l,mlx)-> (msx, mlx, l)) (zip elems allNewStates)
                                    newStates = filter (\st-> (notElem st bmsx) && (notElem st gmsx)) allNewStates
  in ((gmsx++[msx]),(tail bmsx)++newStates,mtrx++newTransitions)


makeDA :: Automation -> Automation
makeDA aut = let (ims,_,mtrx) = makeDA' aut
                 (_,termStates, trans) = foldl (findAnswer aut) ([(ims,1)],[],[]) mtrx
   in (1, sort $ nub termStates, sort trans)

findStNum :: [(MetaState,Int)] -> MetaState -> Maybe Int
findStNum (x:xs) m | fst x == m = Just (snd x)
                   | otherwise = findStNum xs m
findStNum [] _ = Nothing

findAnswer :: Automation ->([(MetaState,Int)],[Int],[Transition])-> MetaTransition -> ([(MetaState,Int)],[Int],[Transition])
findAnswer aut@(_,fs,_) (curTable,termStates,trans) (mFrom,mTo,c) = let fromState = fromMaybe ((snd $ last curTable)+1) (findStNum curTable mFrom)
                                                                        curTable_From = nub $ curTable++[(mFrom,fromState)]
                                                                        toState = fromMaybe ((snd $ last curTable)+1) (findStNum curTable_From mTo)
                                                                        updTable      = nub $ curTable_From++[(mTo,toState)]
                                                                    in if any (\s -> elem s fs) (mTo++(closure aut mTo))
                                                                        then (updTable,toState:termStates,trans++[(fromState,toState,c)])
                                                                        else (updTable,termStates        ,trans++[(fromState,toState,c)])

c10_0, c10_1 , c10_2 , c10_3 , c10_4 , c10_5 , c10_6 , test10 :: Bool
c10_0 = makeDA ndaFigure == daFigure
c10_1 = makeDA nda1 == da1
c10_2 = makeDA nda2 == da2
c10_3 = makeDA nda3 == da3
c10_4 = makeDA nda4 == da4
c10_5 = makeDA nda5 == da5
c10_6 = makeDA nda6 == da6
test10 = c10_0 && c10_1 && c10_2 && c10_3 && c10_4 && c10_5 && c10_6

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
