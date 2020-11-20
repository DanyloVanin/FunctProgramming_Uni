{-# OPTIONS_GHC -Wall #-}
module Vanin06 where

import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Helping functions

adj :: Graph -> Int -> [Int]
adj g v = g !! v

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x<-nodes g, y <- g!!x]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)
stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
stepW _ []  = error "allWays:stepW"

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v],[])

cond :: ([Int],[Int]) -> Bool
cond (ns,_) = ns == []

oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns,os) = (new, old) where
        old = ns ++ os
        ns1 = ns >>= (gr!!)
        ns2 = filter (`notElem` old) ns1
        new = nub ns2


-- Task 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = (checkVertexes gr) && (checkNoDuplicates gr) && (biDirectional gr) && (noLoops gr)
-- no loops > 
noLoops:: Graph -> Bool
noLoops gr = null [v | (v, to) <- zip [0..] gr, elem v to]
-- no vertexes > 
biDirectional:: Graph -> Bool
biDirectional gr = null [v | (v, endpoints) <- zip [0..] gr, to <- endpoints, notElem v (gr !! to)]
-- no duplicates > 
checkNoDuplicates:: Graph -> Bool
checkNoDuplicates gr = null $ filter (\x -> nub x /= x) gr
-- no wrong vertexes > 
checkVertexes:: Graph -> Bool
checkVertexes gr = null [v | g <-gr, v <-g, notElem v (nodes gr)] 

-- Task 2 ------------------------------------  done
fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr -1, edges gr)

-- Task 3 ------------------------------------  done
toGraph :: GraphS -> Graph 
toGraph (vert,eds) = [ list | v<-[0..vert], let list = map (snd) (filter (\x -> v== ((fst x))) eds)]

-- Task 4 ------------------------------------  done
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr from to = let res = (shortWays gr from to) 
                      in if null res then [] else head res

-- Task 5 ------------------------------------   done
isConnecting :: Graph -> Bool 
isConnecting gr = (length gr) == (length $ head $ components gr)

-- Çàäà÷à 6 ------------------------------------
components :: Graph -> [[Int]] 
components gr = nub [sort $ goNodes gr v| v<-nodes gr]

-- Çàäà÷à 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v= maximum $ map (\x -> length x -1) [shortWay gr v x | x <- delete v (nodes gr)]

-- Çàäà÷à 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr = maximum $ map (eccentricity gr) (nodes gr)

findRadius :: Graph -> Int 
findRadius gr = minimum $ map (eccentricity gr) (nodes gr)

-- Çàäà÷à 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = filter (\x -> eccentricity gr x == findRadius gr) (nodes gr)

-- Çàäà÷à 10 ------------------------------------   done
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr from to = let ways = [res | w <- reverse $ allWays gr from, let res = filter (\x -> head x == from && last x == to) (map (reverse) w), not $ null res]
                       in if null ways then [] else head ways


---------------------Òåñòîâ³ äàí³ - Ãðàôè -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
