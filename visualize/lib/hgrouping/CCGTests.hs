module CCGTest where

import Data.TimeInterval
import Data.Graph.Inductive.Graph

import Data.Graph.Grouping.GroupingStructure
import Data.Graph.Grouping.ReebGraph

import Data.Graph.Grouping.ComputeCandidates

import Data.Graph.Inductive.Tree

starts n  = [(i,RData (tm i) Start) | i <- [1..n] ]
stops n k = [(i,RData (tm 100) Stop) | i <- [k+1..(k+n)] ]

singletons n = map (\x -> (x,[x])) [1..n]

initialComps n = singletons [1..n]


pairs []     = []
pairs (x:y:xs) = (x,y) : pairs xs


type CV = (Int,[Int])


mkMerge k ((i,ci),(j,cj)) = ([(i,k,ci),(j,k,cj)], (k,ci++cj))


merges :: [CV] -> Int -> ([REdge], [CV])
merges comps k = foldr (\(x,p) (es,cs) -> let (e,c) = mkMerge x p in
                                          (e ++ es,c:cs)) ([],[]) $ zip [k+1..] (pairs comps)


tm :: Int -> Time
tm = fromIntegral -- i = 10.0 -- i + (0.0 :: Double)

mergeVs :: [CV] -> Int -> [RVertex]
mergeVs comps k = map (\(i,_) -> (i,RData (tm i) Merge)) . snd $ merges comps k

verts :: Int -> [RVertex]
verts n = (starts n) ++ (stops 1 n) ++ (mergeVs (singletons n) (n+1))

edgs n = fst . merges (singletons n) $ n+1


rg :: Gr RData REData
rg = insEdges (edgs 4) . insNodes (verts 4) $ empty


-- mkEdges n k = zipWith3 (\a (i,j) c -> (i,j,a,c)) [k..k+n]






-- rgEdges = concatMap (\(i,j,k,c) )

-- mkEdges (pairs [1..n]) (singletons [1..n])

-- rgVertices n = starts n ++ stops 1 (n+1) ++ mergeVertices
--              where
--                mergeVertices = map








-- -- merges xs k = [(k+i,RData k+i, Merge)  | i <- [0,..,length . pairs $ xs] ]







-- mgVerts c1 c2 t gr = let [i,j,k,l]  = newNodes 4 gr
--                          vs         = [(i,RData t Start)    , (j,RData (t+1) Start),
--                                        (k,RData (t+2) Merge), (l,RData (t+3) S) ]
--                          es         = [(i,k,c1), (j,k,c2), (k,l,c1++c2) ] in
--                      insEdges es . insNodes vs $ gr

-- rg :: Gr RData REData
-- rg = mgVerts [1] [2] 1 empty
