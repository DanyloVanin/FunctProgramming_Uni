{-# OPTIONS_GHC -Wall #-}
module Vanin10 where

import Data.List
-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------  DONE
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b (x:xs) | fst x == a = (a,b):xs
                       | otherwise = x:(updateValue a b xs)
updateValue a b [] = [(a,b)]

--t11,t12,t13,res1 :: Bool
--t11 = updateValue 3 4 [(2,0), (3,7), (1,6), (3,2)] == [(2,0), (3,4), (1,6), (3,2)]
--t12 = updateValue 3 4 [(2,0), (1,6), (0,2)] == [(2,0), (1,6), (0,2), (3,4)]
--t13 = updateValue 3 4 [] == [(3,4)]
--res1 = t11 && t12 && t13
res1 :: Bool
res1 = True
-- Задача 2 ------------------------------------   DONE
updateArray :: Value -> Value -> Value -> Value
updateArray (A arr) (I i) (I val) = A (updateValue i val arr)
updateArray _ _ _ = error "Wrong input"

t21,t22,res2 :: Bool
t21 =  updateArray (lookUp "a" sampleState) (I 2) (I 1) ==  A [(2,1), (0,4), (1,2)]
t22 = updateArray (lookUp "a" sampleState) (I 5) (I 1) ==  A [(2,3), (0,4), (1,2),(5,1)]
res2 = t21 && t22


-- Задача 3 ------------------------------------   DONE
applyOp :: Op -> Value -> Value -> Value 
--Add | Minus | Mul | Less | Equal | Index 
applyOp Add (I v1) (I v2)   = I (v1+v2)
applyOp Minus (I v1) (I v2) = I (v1-v2)
applyOp Mul (I v1) (I v2) = I (v1*v2)
applyOp Less (I v1) (I v2) = if v1<v2 then (I 1) else (I 0)
applyOp Equal (I v1) (I v2) = if v1==v2 then (I 1) else (I 0)
applyOp Index (A arr) (I v2) = let res = filter (\x -> fst x == v2) arr
                               in if null res then (I 0) else I (snd $ head res)
applyOp _ _ _ = error "No such operation"


findFirst :: (a->Bool) -> [a] -> a
findFirst cond xs = case find cond xs of 
  Just a -> a
  Nothing -> error "No suitable value"

t31,t32,t33,t34,t35, t36, res3 :: Bool
t31 = applyOp Add (I 6) (I (-2)) == I 4
t32 = applyOp Mul (I 3) (I 4)  == I 12
t33 = applyOp Less (I 7) (I 0) == I 0
t34 = applyOp Equal (I 2) (I 2) == I 1
t35 = applyOp Index (A [(1,1),(0,3)]) (I 0) == I 3
t36 = applyOp Index (A [(1,1),(0,3)]) (I 2) == I 0
res3 = t31 && t32 && t33 && t34 && t35 && t36 

-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ =I c
evExp (Var v) _ st = snd $ findFirst (\x -> fst x == v) st
evExp (OpApp op exp1 exp2) dfx st = applyOp op (evExp exp1 dfx st ) (evExp exp2 dfx st )
evExp (Cond cond exp1 exp2) dfx st | evExp cond dfx st == (I 0) = evExp exp2 dfx st
                                   | otherwise = evExp exp1 dfx st
evExp (FunApp f es) dfx st = let (_,(as, ef)) = findFirst (\x -> fst x == f) dfx
                                 vs = evArgs es dfx st
                                 new = zip (map (toId) as) vs
                             in evExp ef dfx new      

toId :: VarDef -> Id
toId (Arr i) = i
toId (Int i) = i

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs exps dfx st = map (\e -> evExp e dfx st) exps 

-- testing
t41,t42,t43,t44,t45, res4 :: Bool
t41 = evExp (Const  1) [] sampleState == I 1
t42 = evExp (Var "y") [] sampleState == I 2
t43 = evExp (OpApp Add (Var "x") (Const 2)) [] sampleState == I 7
t44 = evExp (Cond (Const 1) (Var "x") (Const 9)) [] sampleState == I 5
t45 = evExp (FunApp "fib" [Const  6]) [fib] sampleState == I 8
res4 = t41 && t42 && t43 && t44 && t45

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i exp1) dfx _ st = updateValue i (evExp exp1 dfx st) st
evStmt (AssignA i exp1 exp2) dfx _ st = let ind = evExp exp1 dfx st
                                            val = evExp exp2 dfx st
                                            arr = snd $ findFirst (\x -> fst x == i) st
                                            updArr = updateArray arr ind val
                                        in updateValue i updArr st
evStmt (If cond stmt1 stmt2) dfx dpx st = if (evExp cond dfx st) == (I 0) then evStmt stmt2 dfx dpx st
                                               else evStmt stmt1 dfx dpx st
evStmt (While exp1 stmt) dfx dpx st = (snd $ until cond step (evExp exp1 dfx st, st)) where
                                            cond :: (Value,StateP) -> Bool
                                            cond ((I 0),_) = True
                                            cond _ = False
                                            step :: (Value,StateP) -> (Value, StateP)
                                            step (_,nSt) = (evExp exp1 dfx nSt, evStmt stmt dfx dpx nSt)
evStmt (Call i exprs) dfx dpx st = let (defs,stmt) = lookUp i dpx
                                       ids = map (toId) defs
                                       vals = map (\e -> evExp e dfx st) exprs
                                       idVals = zip ids vals
                                       nSt = st++idVals
                                       stmtRes = evStmt stmt dfx dpx nSt
                                    in filter (\x -> notElem (fst x) ids) stmtRes
evStmt (Block defs stmts) dfx dpx st = let vals = map initv defs
                                           ids = map fst vals
                                           nSt = st++vals
                                           stmtRes = recStmtEv stmts dfx dpx nSt
                                        in filter (\x -> notElem (fst x) ids) stmtRes                                  

recStmtEv :: [Stmt] -> [FunDef] -> [ProcDef] -> StateP -> StateP
recStmtEv (stmt:stmts) dfx dpx st = recStmtEv stmts dfx dpx (evStmt stmt dfx dpx st)
--recStmtEv (stmt:[]) dfx dpx st = evStmt stmt dfx dpx st
recStmtEv [] _ _ st = st

t51,t52,t53,t54,res5 :: Bool
t51 =  evStmt sampleBlock  [] [sumA1] [("sA", I 0)] == [("sA", I 22)] --- or just s??
t52 =  evStmt  (Assign "y" (FunApp "sumA" [Var "a", Const 1])) [sumA] [] sampleState ==  [("x", I 5), ("y", I 6), ("a", A [(2,3),(0,4),(1,2)])]
t53 =  evProgram pr2 == [("sA", I 22)]
t54 =  evProgram pr1 == [("gSum", I 15)]
res5 = t51 && t52 && t53 && t54 
-- Задача 6 ------------------------------------
iswfOpJust :: Op -> [Maybe Type] -> Maybe Type 
iswfOpJust Add   [Just It,Just It] = Just It
iswfOpJust Minus [Just It,Just It] = Just It
iswfOpJust Mul   [Just It,Just It] = Just It
iswfOpJust Less  [Just It,Just It] = Just It
iswfOpJust Equal [Just It,Just It] = Just It
iswfOpJust Index [Just At,Just It] = Just It
iswfOpJust _      _      = Nothing

iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Const _) _ _ = Just It
iswfExp (Var i) ve _ = case find (\x -> fst x == i) ve of
                       Just t -> Just (snd t)
                       Nothing -> Nothing
iswfExp (OpApp op exp1 exp2) ve fe = iswfOpJust op [(iswfExp exp1 ve fe),(iswfExp exp2 ve fe)] 
iswfExp (Cond exp1 exp2 exp3) ve fe = case ((iswfExp exp1 ve fe),(iswfExp exp2 ve fe),(iswfExp exp3 ve fe)) of
  (Just It, Just It, Just It) -> Just It
  (Just It, Just At, Just At) -> Just At
  _                           -> Nothing
iswfExp (FunApp i exps) ve fe = let func = map Just (lookUp i fe)
                                    expRes = map (\x -> iswfExp x ve fe) exps
                                in if func == expRes then Just It else Nothing

t61,t62,t63,t64, res6 :: Bool
t61 = iswfExp (Var "a") varEnv  [] == Just At
t62 = iswfExp (Var "b") varEnv  [] == Nothing
t63 = iswfExp (FunApp "fib" [ Var "x"]) varEnv [("fib",[It])] ==  Just It
t64 = iswfExp (FunApp "fib" [Const 6, Var "x"]) varEnv [("fib",[It])] == Nothing  
res6 = t61 && t62 && t63 && t64         

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i exp1) ve fe _ = let var = lookup i ve
                                       expRes = iswfExp exp1 ve fe
                                   in (var == expRes) && (var==Just It)
iswfStmt (AssignA i exp1 exp2) ve fe _ = let Just var = lookup i ve
                                             Just resExp1 = iswfExp exp1 ve fe
                                             Just resExp2 = iswfExp exp2 ve fe
                                         in iswfAssignA [var,resExp1,resExp2]
iswfStmt (If exp1 stmt1 stmt2) ve fe pe = let valExp = (Just It == iswfExp exp1 ve fe)
                                              valStmt1 = iswfStmt stmt1 ve fe pe
                                              valStmt2 = iswfStmt stmt2 ve fe pe
                                          in valExp && valStmt1 && valStmt2
iswfStmt (While exp1 stmt1) ve fe pe = let valExp = (Just It == iswfExp exp1 ve fe)
                                           valStmt1 = iswfStmt stmt1 ve fe pe
                                       in valExp && valStmt1
iswfStmt (Call i exps) ve fe pe = let st = map Just (lookUp i pe)
                                      expRes = map (\x -> iswfExp x ve fe) exps
                                  in st == expRes
iswfStmt (Block vars stmts) ve fe pe = let newVars = (map initIdType vars) ++ ve
                                       in all (\x -> iswfStmt x newVars fe pe) stmts 

initIdType :: VarDef -> (Id, Type)
initIdType (Arr v) = (v, At)
initIdType (Int v) = (v, It)

t71,t72,t73,res7 :: Bool
t71 = iswfStmt  sampleBlock varEnv [] procEnv == True
t72 =  iswfStmt  (Assign "y" (FunApp "sumA" [Var "a", Var "x"])) varEnv funEnv [] == True
t73 =  iswfStmt  (Assign "y" (FunApp "sumA" [Var "y", Var "x"])) varEnv funEnv [] == False
res7 = t71 && t72 && t73


-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (i,(vars,exp1)) fe = let idTypes = (i, map initType vars)
                                    newVarEnv = map initIdType vars
                                in (elem idTypes fe) && (iswfExp exp1 newVarEnv fe /= Nothing) 

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (i,(vars, stmt)) ve fe pe = let varTypes = map initType vars
                                            newVe = map initIdType vars ++ ve
                                            newPe = updateValue i varTypes pe
                                        in elem (i,varTypes) newPe && (iswfStmt stmt newVe fe newPe)

initType :: VarDef -> Type
initType (Arr _) = At
initType (Int _) = It

-- testing
t81, t82, t83, t84, res8 :: Bool
t81 =   iswfFunDef fib funEnv == True
t82 =  iswfProcDef sumA1 varEnv funEnv procEnv == False 
t83 =  iswfProcDef sumA1 [("sA",At)] funEnv procEnv == False
t84 =  iswfProcDef sumA1 [("sA",It)] [] []  == True 
res8 = t81 && t82 && t83 && t84

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (ve,fe,pe) = let veEnv = map initIdType ve
                             feEnv = makeEnv fe
                             peEnv = makeEnv pe
                             isFuncts = all (\x -> iswfFunDef x feEnv) fe
                             isProcs = all (\x -> iswfProcDef x veEnv feEnv peEnv) pe
                             hasMain = any (\(i,_) -> i == "main") peEnv
                             allNamesUnique = allUnique (toNames veEnv feEnv peEnv)
                          in isFuncts && isProcs && hasMain && allNamesUnique

allUnique :: [Id] -> Bool
allUnique ids = nub ids == ids

toNames :: VarEnv -> FunEnv -> ProcEnv -> [Id]
toNames ve fe pe = let getAllNames e = map (\(name, _) -> name) e
                   in (getAllNames ve) ++ (getAllNames fe) ++ (getAllNames pe)

makeEnv :: [(Id, ([VarDef], a))] -> [(Id,[Type])]
makeEnv fd = map defToEnv fd where 
              defToEnv (i,(vars,_)) = (i, map initType vars)

-- testing
t91,t92,res9 :: Bool
t91 = iswfProgram pr1 == True
t92 = iswfProgram pr2 == True
res9 = t91 && t92

total :: Bool
total = res1 && res2 && res3 && res4 && res5 && res6 && res7 && res8 && res9


--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
