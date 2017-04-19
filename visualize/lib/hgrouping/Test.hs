module Test.GT where

import Data.TimeInterval

import Data.Graph.Grouping.Types
import Data.Graph.Grouping.GroupingTree

import Data.Graph.Components.DynamicConnectedComponents
import Data.Graph.Components.SimpleDynamicConnectedComponents
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph(LNode,Node)


------------------------
-- CTRees

group xs s e = Group xs (Interval (s,e))

gs = [ group [1] 1 10
     , group [2] 1 10
     , group [3] 1 10
     , group [4] 1 10
     , group [5] 1 10
     ]


gs1 = [ group [1,2] 2 10
      , group [3,4] 3 10
      ]

gs2 = [group [1..4] 4 10]

gs3 = [group [1..5] 5 10]

trees :: [CTree]
trees = map singleton gs

gr i = trees !! (i-1)

t1 = merge2 2 (gr 1) (gr 2)
t2 = merge2 3 (gr 3) (gr 4)

t3 = merge2 4 t1 t2
t4 = merge2 5 t3 (gr 5)






------------------------------
--dynamic components


type CData   = Int
type CVertex = LNode CData
type CComp   = [Node] -- This is a bit of a hack, since a (C)Node directly corresponds to an entity
type DynamicComponents = SimpleComponents CData ()


initF :: Int -> DynamicComponents
initF n = initialise . map (\t -> (t,t)) $ [1..n]

f,f1,f2,f3,f4 :: DynamicComponents
f = initF 5

f1 = insEdge (1,2) f
f2 = insEdge (2,3) f1
f3 = insEdge (3,1) f2

f4 = delEdge (2,3) f3

f5 = insEdge (2,3) f4
f6 = insEdge (1,5) f5
f7 = insEdge (4,5) f6

f5' = insEdge (4,5) f4
