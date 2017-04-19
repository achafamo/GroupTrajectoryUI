module Data.Graph.Components.DynamicConnectedComponents where

import Data.Graph.Inductive.Graph(LNode,Edge,Node,LEdge)
import Data.Graph.Inductive.Tree
import Data.Set

type Component = Set Node


class MaintainsComponents f where

    initialise :: [LNode a] -> f a b

    fullGraph  :: f a b -> Gr a b

    insLEdge :: LEdge b -> f a b -> f a b
    delLEdge :: LEdge b -> f a b -> f a b

    insEdge :: Edge -> f a () -> f a ()
    insEdge (u,v) = insLEdge (u,v,())
    delEdge :: Edge -> f a () -> f a ()
    delEdge (u,v) = delLEdge (u,v,())


    component  :: Node -> f a b -> Component
    components :: f a b -> [Component]
