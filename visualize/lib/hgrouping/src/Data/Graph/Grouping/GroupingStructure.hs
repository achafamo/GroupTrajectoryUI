{-# Language
             ExistentialQuantification,
             TypeFamilies,
             FlexibleInstances,
             FlexibleContexts,
             GADTs,
             KindSignatures,
             UndecidableInstances
 #-}
module Data.Graph.Grouping.GroupingStructure where

import Data.Trajectory
import Data.TimeInterval

import Data.Graph.Grouping.Types
import Data.Graph.Grouping.ReebGraph
import Data.Graph.Grouping.BuildReebGraph
import Data.Graph.Grouping.ComputeCandidates

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Debug.Trace

-----------------------------------------------------------------------------------
-- | The main algorithm

groups                  :: Parameters -> [Trajectory] -> [CGElem CGroupStruct]
groups ps@(Parameters eps delta m) =  groups' ps . buildReebGraph eps

groups'    :: Graph gr => Parameters -> gr RData REData ->
              [CGElem CGroupStruct]
groups' ps = filter (isGroup ps) . candidates

isGroup                          :: CanStoreEntities c => Parameters -> Group c -> Bool
isGroup (Parameters _ delta m) g = duration g >= delta && groupSize g >= m
