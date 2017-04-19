module Data.Graph.Components.SimpleDynamicConnectedComponents(SimpleComponents)
    where

import Data.Graph.Components.DynamicConnectedComponents
import Data.Graph.Inductive.Graph(Node,LNode,LEdge)
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query.DFS
import Data.Maybe

import Data.Map(Map)
import Data.Set(Set)
import Data.List
import Data.Ord

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph.Inductive.Graph as G


import Debug.Trace

type Comps = Map Node Node

-- | simple implementation that works for UNdirected graphs. The components are stored as
-- a Map from nodeId to the nodeId of the node representing the component. Hence the map
-- represents a forest.
data SimpleComponents v e = SimpleComponents { gr :: Gr v e
                                             , comps :: Comps
                                             }
                          deriving (Show)


-- insert underected edge in the graph
insUEdge e@(u,v,d) = G.insEdges [e,(v,u,d)]
-- delete undriected edge in the graph
delUEdge e@(u,v,_) = G.delEdges [(u,v),(v,u)]


jLookup   :: Ord k => k -> Map k v -> v
jLookup k = fromJust . M.lookup k


-- test whether two components are the same, e.g. have the same representative
sameComp       :: Node -> Node -> SimpleComponents v e -> Bool
sameComp v u f = let m = comps f in jLookup v m == jLookup u m

--  insert a new edge in the graph, do nothing in the components
insEdge' :: LEdge e -> SimpleComponents v e -> SimpleComponents v e
insEdge' e (SimpleComponents g m) = SimpleComponents (insUEdge e g) m




-- get alle elements in the component represented by r
comp   :: Node -> Comps -> Component
comp r = S.fromList . M.keys . M.filter (== r)

-- set the component for all nodes ns to rep
updateComponent          :: Component -> Node -> Comps -> Comps
updateComponent ns rep m = S.foldr (\n m' -> M.insert n rep m') m ns

instance MaintainsComponents SimpleComponents where

    initialise vs = SimpleComponents g m
        where
          g = G.insNodes vs (G.empty :: Gr v e)
          m = M.fromList [(v,v) | v <- G.nodes g]

    fullGraph (SimpleComponents g _) = g

    insLEdge e@(v,u,_) f | sameComp v u f       = insEdge' e f
    insLEdge e@(v,u,_) f@(SimpleComponents g m) = SimpleComponents g' m'
        where
          g' = insUEdge e g
          m' = updateComponent (component u f) (jLookup v m) m

    delLEdge e@(v,u,_) (SimpleComponents g m) = (SimpleComponents g' m')
        where
          g'   = delUEdge e g
          cmpv = S.fromList $ reachable v g'
          cmpu = S.fromList $ reachable u g'
          r    = jLookup v m -- the old representative
          m' = case (u `S.member` cmpv, r `S.member` cmpv) of
                 (True,_) -> m -- u and v still in one component
                 (_,True) -> updateComponent cmpu u m -- relabel stuff in the u componentx
                 _        -> updateComponent cmpv v m -- relabel stuff in the v component



    component n (SimpleComponents g m) = comp (jLookup n m) m

    components f = [ S.fromList . map fst $ g | g <- groupBy repr $ xs]
        where
          repr x y  = snd x == snd y
          xs        = sortBy (comparing snd) . M.toList . comps $ f
