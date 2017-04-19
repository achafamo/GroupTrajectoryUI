module RGTest where

import Data.TimeInterval
import Data.Graph.Grouping.ReebGraph
import Data.Graph.Grouping.BuildReebGraph
import Data.Graph.Grouping.ComputeCandidates

import Data.Graph.Grouping.GroupingStructure

import Data.Graph.Inductive.Tree

import Data.Trajectory

import Debug.Trace

t1 = Trajectory 1 [TP (300,300,10), TP (400,400,12)]
t2 = Trajectory 2 [TP (300,400,10), TP (400,300,12)]

trajecs = [t1,t2]

rg :: Gr RData REData
rg = buildReebGraph 0.1 trajecs

params = (0.1,0,2)


grs' = candidates 0.1 rg

grs = groups params trajecs

main = do
         putStr "groups found:\n--------------\n"
         mapM_ print grs'
