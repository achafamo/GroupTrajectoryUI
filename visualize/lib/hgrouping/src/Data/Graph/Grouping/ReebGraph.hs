{-# Language
             TypeSynonymInstances,
             ExistentialQuantification,
             FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             GADTs
 #-}
module Data.Graph.Grouping.ReebGraph where

import Control.Monad

import Data.Trajectory
import Data.TimeInterval
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Set (Set)

import Data.Graph.Grouping.Types
import Data.Graph.Grouping.GroupingTree

import qualified Data.Set as S
import qualified Data.Foldable as F

-- | The data structures we use to store components and candidate groups
type ComponentStruct  = Set
type CGroupStruct     = CTree


type Component = ComponentStruct Entity

--------------------------------------------------------------------------------
-- | Types for the Reeb graph

-- | The vertices in the ReebGraph are start,merge,split, or stop vertices
data VType = Start | Merge | Split | Stop deriving (Show,Read,Eq)

-- | A vertex in the reeb graph has an associated time and a type
data RData = RData Time VType
           deriving (Show,Eq,Read)

rType (RData _ t) = t

instance HasTime RData where
    time (RData t _) = t


-- | The Vertex type used in the graph
type RVertex = LNode RData

instance HasTime RVertex where
    time (_,d) = time d

-- | Initially, the edges in the reeb graph are labled with a component
type REData    = Component
type REdge     = LEdge REData

-- | Given a graph, get the time interval on which we are active
-- edgeInterval            :: Graph gr => gr RData b -> b -> Interval

edgeInterval            :: (Graph gr, HasTime a) => gr a b -> LEdge b -> Interval
edgeInterval gr (u,v,_) = Interval (getT u, getT v)
                          where
                            getT = time . fromJust . (lab gr)


-- | After computing the candidate groups, edges in the Reeb graph are labled
-- with the component and their candidate groups
type ROData   = (Component, CGroupStruct)
type ROutEdge = LEdge ROData
