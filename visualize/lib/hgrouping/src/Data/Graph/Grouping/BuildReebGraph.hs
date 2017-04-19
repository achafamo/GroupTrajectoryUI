{-# Language
            TypeFamilies,
            FlexibleContexts,
            GADTs
  #-}
module Data.Graph.Grouping.BuildReebGraph( computeEvents
                                         , buildReebGraph
                                         , buildReebGraph'
                                         ) where

import Data.Trajectory
import Data.Maybe
import Data.List
import Data.Set(Set)
import Data.Trajectory(Trajectory)
import Data.TimeInterval
import Data.Map(Map)

import Data.Graph.Components.SimpleDynamicConnectedComponents
import Data.Graph.Components.DynamicConnectedComponents(MaintainsComponents(..))

import Data.Graph.Grouping.Event
import Data.Graph.Grouping.Types
import Data.Graph.Grouping.ReebGraph(RVertex, REdge, RData(..),REData,Component)
import Data.Graph.Inductive.Graph(Graph,LNode,Edge,Node,mkGraph)

import Data.Graph.Inductive.Tree

import qualified Data.Trajectory as T
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Grouping.ReebGraph as RG
import qualified Data.Graph.Grouping.Event as E
import qualified Data.Map as M


import Debug.Trace

-- TODO
type DynamicComponents = SimpleComponents CData ()


jLookup   :: (Show k,Show v, Ord k) => k -> Map k v -> v
--jLookup k m | traceShow (k,m) False = undefined
jLookup k m = fromJust . M.lookup k $ m


-- TODO: we can gain a speedup by computing the events in a different order
-- i.e. compute all events for all first trajectory edges, etc.
-- this gives O(tau*n log n) instead of O(tau*n log (tau*n))

-- | compute all event points for a set of trajectories
computeEvents        :: Double -> [Trajectory] -> [Event]
computeEvents eps ts = concatMap (events' eps) [(t1,t2) | t1 <- ts, t2 <- ts, T.tid t1 < T.tid t2]

events' eps (t1,t2) = nub . concatMap f $ zip (T.edges t1) (T.edges t2)
                      where
                        f = uncurry $ E.events (2*eps) (tid t1) (tid t2)

buildReebGraph :: Double -> [Trajectory] -> Gr RData REData
buildReebGraph = buildReebGraph'

buildReebGraph'        :: Graph gr => Double -> [Trajectory] -> gr RData REData
buildReebGraph' eps ts = mkGraph vs es
    where
      -- number of trajectories
      n        = length ts
      -- get the (global) start and stop times of the input trajectories
      (st,tt)  = let t = head ts in (T.start t, T.stop t)
      -- compute the initial components
      initF    = initComponents eps ts
      -- create the initial mapping between components and vertices in the reeb graph
      initMap  = M.fromList . map (\(i,c) -> (c,(i,RData st RG.Start))) .
                   zip [1..] . components $ initF :: Map CComp RVertex
      -- In our state we only need a map to Nodes
      initMap' = M.map fst initMap
      -- start vertices
      starts   = M.elems initMap
      -- the events
      evts     = sort . computeEvents eps $ ts
      -- Compute all remaining vertices and edges
      (vs,es)  = handleEvents tt evts (ST initF starts [] initMap' (n + 1))



-- | The of vertices and data in the graph G, for which we maintain connected components
type CData   = Entity
type CVertex = LNode CData
type CComp   = Component -- This is a bit of a hack, since a (C)Node directly corresponds to an entity

type CEdge = G.Edge

-- | initialise the dynamic components
initComponents        :: Double -> [Trajectory] -> DynamicComponents
initComponents eps ts =  foldr insEdge emptyF initEs
    where
      -- the trajectory ids
      tids      = map T.tid ts
      -- get the starting point per entity
      startPs   = M.fromList [ (T.tid t, T.startPoint t) | t <- ts]
      -- convenience wrapper to find the start point for a vertex
      startP vi = jLookup vi startPs
      -- vertices in the components graph
      gvs       = [(i,i) | i <- tids]
      -- init the structure to maintain connected components in gvs
      emptyF    = initialise gvs :: DynamicComponents
      -- initial edges (i.e. vertices that are initially directly connected)
      initEs    = [(v,u) | v <- tids, u <- tids, v /= u,
                           isDirectlyConnected (2*eps) (startP v) (startP u)]


isDirectlyConnected eps p q = dist p q <= eps


-- | During construction we represent the reeb graph as a list of vertices and a list
-- of edges
type RG = ([RVertex],[REdge])

data ST f = ST { comStruct  :: f CData ()
               , rgVertices :: [RVertex]
               , rgEdges    :: [REdge]
               , mapping    :: Map CComp Node
               , firstLabel :: Node
               }

handleEvents :: MaintainsComponents f => Time -> [Event] -> ST f -> RG
handleEvents stopt []     st                    = addStops stopt st
handleEvents stopt (e:es) st | eKind e == Join  = handleEvents stopt es (handleJoin  e st)
handleEvents stopt (e:es) st | eKind e == Split = handleEvents stopt es (handleSplit e st)


addStops                         :: MaintainsComponents f =>
                                    Time -> ST f -> RG
addStops stopt (ST _ vs es m vi) = (vs',es')
    where
      (vs',es',_)             = M.foldrWithKey add (vs,es,vi) m
      add c vc (vs'',es'',ui) = let u = (ui,RData stopt RG.Stop) in
                                (u:vs'',(vc,ui,c):es'',ui+1)


handleJoin (Event (x,y,t,_)) (ST f vs es m vi) = let
                                                   cx = component x f
                                                   cy = component y f
                                                   vx = jLookup cx m
                                                   vy = jLookup cy m
                                                   f' = insEdge (x,y) f in
    if cx == cy then
                  ST f' vs es m vi
                else let
                       vs' = (vi,RData t RG.Merge) : vs
                       es' = [(vx,vi,cx),(vy,vi,cy)] ++ es
                       m'  = M.insert (unions [cx,cy]) vi . M.delete cx . M.delete cy $ m in
                  ST f' vs' es' m' (vi+1)


--handleSplit e st | traceShow e False = undefined
handleSplit (Event (x,y,t,Split)) (ST f vs es m vi) = let
                                                        c  = component x f
                                                        ui = jLookup c  m
                                                        f' = delEdge (x,y) f
                                                        cx = component x f'
                                                        cy = component y f' in
    if cx == cy then
                  ST f' vs es m vi
                else let
                       vs' = (vi,RData t RG.Split) : vs
                       es' = (ui,vi,c) : es
                       m'  = M.insert cx vi . M.insert cy vi . M.delete c $ m in
                  ST f' vs' es' m' (vi+1)
