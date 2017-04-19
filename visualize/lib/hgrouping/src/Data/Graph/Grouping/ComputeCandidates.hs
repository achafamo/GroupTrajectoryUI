{-# Language
            TypeFamilies
  #-}
module Data.Graph.Grouping.ComputeCandidates(candidates) where

import Data.TimeInterval

import Data.Foldable(Foldable)
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Set (Set)
import Data.Graph.Grouping.GroupingTree

import Data.Graph.Grouping.Types
import Data.Graph.Grouping.ReebGraph

-- Note this determines what graph implementation we use
import Data.Graph.Inductive.Tree

import Debug.Trace

import qualified Data.Set as S

thrd (_,_,x) = x

eData :: LEdge b -> b
eData = thrd

cands :: ROutEdge -> CGroupStruct
cands = snd . eData

candidates' :: (Graph gr, DynGraph dgr) =>
              [Node]                -> -- The vertices and in which order to handle them
              gr RData REData   -> -- the input graph
              dgr RData ROData -> -- the accumulated output graph so far
                    dgr RData ROData  -- output graph in which edges are labled
                                              -- with their candidate groups
candidates' []      _  g'              = g'
candidates' _       gi g' | isEmpty gi = g'
candidates' (vi:vs) gi g'              = case match vi gi of
                                           (Nothing, _) -> error "this should not happen"--candidates' vs gi g'
                                           (Just c,  _) -> candidates' vs gi $! (grow c)
    where
      grow co@(ps,_,d,ss) = let v = (vi,d)
                                t = time d in case rType d of
        -- at a start vertex we create a new CGRoupStruct for the component c
        -- and label the new edge e with it
        Start -> let [(c,j)] = ss
                     gr      = Group c (Interval (t,t))
                     e       = (vi,j,(c,singleton gr :: CGroupStruct))  in
                 insEdge e g'
        -- at a merge vertex we simply gather the incoming candidate groups
        -- and use `merge' to construct the cand-groups on the outgoing
        -- edge
        Merge -> let [(c,j)] = ss
                     incands = map cands . inn g' $ vi
                     e       = (vi,j,(c, merge t incands)) in
                 insEdge e g'
        -- get the component ci and the candidate groups icands on the incoming edge
        -- then for each outgoing edge simply extract the candidate groups
        Split -> let [(ci,_)] = ps
                     icands   = cands . head . inn g' $ vi -- cand groups on incoming edge
                     es       = map (\(c,j) -> (vi,j,(c,extract c icands))) ss in
                 insEdges es g'
        -- At a stop vertex we don't have to do anything, since there are no outgoing
        -- edges.
        Stop  -> g'


-- | traverse the graph and simply collect all candidate groups
collect :: Graph gr =>
            [Node] -> -- The vertices and in which order to handle them
            gr RData ROData ->  -- the graph
            [CGElem CGroupStruct] ->  -- the accumulated groups so far
              [CGElem CGroupStruct]
collect []      g gs             = gs
collect _       g gs | isEmpty g = gs
collect (vi:vs) g gs             = case match vi g of
                                     (Nothing, _) -> error "collect" -- this should not happen
                                     (Just c,  _) -> collect vs g (col c ++ gs)
    where
      col co@(ps,_,d,ss) = let v = (vi,d)
                               t = time d in
                           case rType d of
                             Split -> let [((_,cg),_)] = ps -- incoming candidate groups
                                          ((c, _),_)   = head ss in -- an outgoing component
                                      ended t c cg
                             Stop  -> let [((_,cg),_)] = ps in
                                      listGroups t cg
                             _     -> [] -- groups only end at split or stop vertices


candidates :: Graph gr =>
              gr RData REData    -> -- ^ the Reeb graph
                 [CGElem CGroupStruct]
candidates g = collect order g' []
               where
                 order  = topsort g
                 empty' = empty :: Gr RData ROData
                 ginit  = insNodes (labNodes g) empty'
                 g'     = candidates' order g ginit
