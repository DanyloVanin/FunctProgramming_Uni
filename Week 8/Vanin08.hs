{-# OPTIONS_GHC -Wall #-}
module Vanin08 where
import Data.List
import Text.ParserCombinators.Parsec

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Çàäà÷à 1 ------------------------------------    DONE
isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst syst (Super Succ [f]) = isNumbConst syst f
isNumbConst syst (Name name) = case findFunctionByName syst name of
  (Just (_,f)) -> isNumbConst syst f
  Nothing -> False
isNumbConst _ _ = False

findFunctionByName :: System -> String -> Maybe(String,Recur)
findFunctionByName syst name = find (\r -> fst r == name) syst
-- Çàäà÷à 2 ------------------------------------    DONE
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st) - 1
evRank syst (Mini b _) = (evRank syst b) - 1
evRank syst (Name name) = case findFunctionByName syst name of
  (Just (_,f)) -> evRank syst f
  Nothing -> -1

-- Çàäà÷à 3 ------------------------------------   DONE
isNames :: System -> Bool 
isNames syst = (checkUniqueNames names) && (all (checkMadeFromPrevious names) syst) where 
  names = fst $ unzip syst

checkUniqueNames :: [String] -> Bool
checkUniqueNames names = (length names) == (length $ nub names)

checkMadeFromPrevious :: [String] -> (String, Recur) -> Bool
checkMadeFromPrevious names (currName, f) = let usedNames = nub $ getUsedNames f in
 if null usedNames then True 
 else (all (compareToCurr (elemIndex currName names)) (map (\x -> elemIndex x names) usedNames))

compareToCurr :: Maybe Int -> Maybe Int -> Bool
compareToCurr (Just curr) (Just used) = used < curr
compareToCurr _ _ = False 

getUsedNames :: Recur -> [String]
getUsedNames (Prim i st) = (getUsedNames i) ++ (getUsedNames st)
getUsedNames (Mini b _) = (getUsedNames b)
getUsedNames (Super b al) = (getUsedNames b) ++ (concatMap (getUsedNames) al)
getUsedNames (Name name) = [name]
getUsedNames _ = []

-- Çàäà÷à 4 ------------------------------------  DONE
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel i j) = (i>=1) && (j>=1) && (j<=i)
isRecur syst (Super f al) = (rank == length al) && (allTheSame (map (evRank syst) al)) && (all (isRecur syst) (f:al)) where 
  rank = evRank syst f 
isRecur syst (Prim g h) | (isNumbConst syst g) = ((evRank syst h) == 2) && ((evRank syst g) == 1) && (isRecur syst g) && (isRecur syst h) 
                        | otherwise = ((evRank syst h) - (evRank syst g) == 2) && (isRecur syst g) && (isRecur syst h)
isRecur syst (Mini g _) = ((evRank syst g) > 1) && (isRecur syst g)
isRecur syst (Name nm) = case (findFunctionByName syst nm) of
  (Just (_,f)) -> isRecur syst f
  Nothing -> False

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

c4,cc4 :: Bool
c4 = isRecur syst1 (Mini (Name "subtractionAbs3") 100)
cc4 = isRecur syst1 (Mini (Name "substractionAbs3") 100)

-- Çàäà÷à 5 ------------------------------------   DONE

getFunction :: System -> String -> Recur
getFunction syst name = case find (\r -> fst r == name) syst of
  Nothing -> error "No such function found"
  Just (_,f) -> f

eval :: System -> Recur -> [Int] -> Int 
eval _ (Zero) _ = 0 
eval _ (Succ) vl = head vl + 1
eval _ (Sel _ i) vl = vl !! (i-1)
eval syst (Super f al) vl = eval syst f (map (\r-> eval syst r vl) al) 
eval syst (Name name) vl =  eval syst (getFunction syst name) vl  
eval syst r@(Prim _ _) vl = evalPrim syst r vl
eval syst minR@(Mini _ _) vl = case evalPart syst minR vl of 
    Just res -> res
    Nothing -> 0

evalPrim :: System -> Recur -> [Int] -> Int
evalPrim syst (Prim r1 r2) vl = last $ fst ( until cond (stepEvalPrim syst r2) (init vl++[0]++[eval syst r1 vl], last vl) )
evalPrim _ _ _ = error "Incorrect function usage"

cond::([Int],Int)->Bool
cond (xs,stopLimit) = stopLimit <= (last $ init xs)

stepEvalPrim::System -> Recur -> ([Int],Int) -> ([Int],Int)
stepEvalPrim syst r1 (vl,stopLimit) = (leftPart ++ [counter+1]++[eval syst r1 (leftPart++[counter]++[last vl])], stopLimit) where 
                                      counter  = last $ init vl
                                      leftPart = init $ init vl

c51,c52 :: Bool
c51 = eval syst1 (Name "addition") [6, 34] == 40
c52 = eval syst1 (Super Succ [Super Succ [Zero]]) [60] == 2

-- Çàäà÷à 6 ------------------------------------     DONE
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart syst r@Zero vl = Just $ eval syst r vl
evalPart syst r@Succ vl = Just $ eval syst r vl
evalPart syst r@(Sel _ _) vl = Just $ eval syst r vl
evalPart syst r@(Super _ _) vl = Just $ eval syst r vl
evalPart syst r@(Prim _ _) vl = Just $ eval syst r vl
evalPart syst (Name name) vl = evalPart syst (getFunction syst name) vl
evalPart syst (Mini g t) vl = evalMini syst g t (vl++[0])

evalMini :: System -> Recur -> Int -> [Int] -> Maybe Int
evalMini syst g t v | last v > t = Nothing
                    | otherwise = case evalPart syst g v of
                                    Nothing -> Nothing
                                    Just 0  -> Just (last v)
                                    Just _  -> evalMini syst g t ((init v) ++ [last v +1])

c61,c62 :: Bool
c61 = evalPart syst1 (Name "subtractionPart") [6,3] == Just 3
c62 = evalPart syst1 (Name "subtractionPart") [6, 34] == Nothing



-- Çàäà÷à 7 ------------------------------------   DONE
parseRec :: String -> Maybe System 
parseRec str = case parse system "" (filter (\x -> (notElem x "\t\n \\")) str) of 
    Right syst -> Just syst
    Left _ -> Nothing

integer :: Parser Int
integer = do ds <- many1 digit
             return $ read ds
 
nameR :: Parser Recur
nameR = do s <- letter
           rest <- many (letter <|> digit) 
           return (Name (s:rest))

iden :: Parser String
iden = do c <- letter
          st <- many (letter <|> digit) 
          return (c:st)

recur :: Parser Recur
recur = base <|> super <|> prim <|> mini

base :: Parser Recur
base =(try succ1)<|>(try zero)<|>(try sel)<|> nameR

super :: Parser Recur
super = do _<- char '('
           r1 <- recur
           _<- char ':'
           r2 <- recur 
           rest <- many ((char ',') >>recur) 
           _<- char ')'
           return (Super r1 (r2:rest))

prim :: Parser Recur
prim = do _<- char '['
          r1 <- recur
          _<- char ','
          r2 <- recur 
          _<- char ']'
          return (Prim r1 r2)

mini :: Parser Recur
mini = do _<- char '{'
          r1 <- recur
          _<- char ','
          r2 <- integer 
          _<- char '}'
          return (Mini r1 r2)

succ1 :: Parser Recur
succ1 = do _ <- string "a1"
           return (Succ)    

zero :: Parser Recur
zero = do _ <- string "z1"
          return (Zero)

sel :: Parser Recur
sel = do _<-char 's'
         i1 <- digit
         i2 <- digit
         return (Sel (read [i1]) (read [i2]))

expret::Parser (String,Recur)
expret = do str<-iden
            _<- char '='
            r<-recur
            _<-char ';'
            return (str,r)

system::Parser System
system = do res<-many1 expret
            eof
            return (res)

---------------------Òåñòîâ³ äàí³ -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
