module Data.Graph.Components.EulerTree where

import Data.Maybe
import Data.RBTree(RBTree)
import Data.Map(Map)

import qualified Data.RBTree as RB
import qualified Data.Map as M

type NodeIdx = Int
type Offset = Int

-- | A node (nodeId, representative)
newtype Node = Node (NodeIdx,NodeIdx)
    deriving (Show,Eq)

instance Ord Node where
    x `compare` y = if x == y then EQ else if nix x < nix y then LT else GT

unN (Node t) = t

-- fst3 (a,b,c) = a
-- snd3 (a,b,c) = b
-- thd3 (a,b,c) = c

nix = fst . unN
repr = snd . unN


uLookup k = fromJust . M.lookup k

----------------------------------------------------------------------------

-- | An euler tree T_e represents the euler tour around a tree T. We refer to
-- the nodes in T as vertices and edges. The nodes in T_e are called nodes,
-- which are connected by arcs. The eulerTree T_e has three fields: tree: The
-- tree represents the (closed) euler path. Every node in the tree corresponds
-- to a node on the euler path. Hence we may visit each vertex multiple
-- times. To save space each vertex has a representative node.
-- vertices: map from the representative to the vertices
-- reprs :: map back from the vertex to the repr.
-- lastUsed last used node
data EulerTree a = EulerTree { tour     :: RBTree Node
                             , reprs    :: Map a NodeIdx      -- value -> repr
                             , vertices :: Map NodeIdx a      -- repr -> value
                             , lastUsed :: NodeIdx
                             }
                 deriving (Show)


-- TODO: think about this. This is what we need in our impl. But is it also the most logical
-- thing to do in general?
instance Eq a => Eq (EulerTree a) where
    t1 == t2 = root t1 == root t2


singleton x = EulerTree (RB.fromList $ [Node (0,0), Node (1,0)])
                        (M.singleton x 0)
                        (M.singleton 0 x)
                        1

-- close the tour
close        :: Int -> RBTree Node -> RBTree Node
close size t = let (Node (i,r)) = RB.minimum t in
               RB.insert (Node (size,r)) t


-- the root of the tree T that we represent
root :: EulerTree a -> a
root t = let r = nix . RB.minimum . tour $ t in
         fromJust . M.lookup r . vertices $ t


-- shift all indices and indices of representatives k to the right
-- if k < 0 we shift to the left
shiftTree k = RB.map (\(i,r) -> (i+k,r+k))
shiftReps k = M.map (+k)
shiftVert k = M.mapKeysMonotonic (+k)


size :: EulerTree a -> Int
size = (+1) . lastUsed

split                         :: Ord a => a -> EulerTree a -> (EulerTree a, EulerTree a)
split v (EulerTree t rs vs l) = (o1,o2)
    where
      r         = uLookup v rs -- get the representative of this vertex
      r'        = r+1
      (t1',t2') = split r t    -- split the tour just after visiting the representative
                               -- shift the right tree to the left and close the paths
      (t1, t2)  = (close t1',close $ shiftTree -r' t2')
      (rs1,rs2) = (M.filter (<=r) rs, shiftReps -r' $ M.filter (>r) rs)
      (vs1,vs2) = (M.filterWithKey ((<= r) . fst) vs,
                   shiftVert -r' $ M.filterWithKey ((>r) . fst) vs)
      (o1,o2)   = (EulerTree t1 rs1 vs1 r',EulerTree t2 rs2 vs2 l-r')


-- join :: (a,EulerTree a) -> (a,EulerTree a) -> EulerTree a
-- join (v,EulerTree t r vs l) (u,EulerTree t' r' vs' l') =
