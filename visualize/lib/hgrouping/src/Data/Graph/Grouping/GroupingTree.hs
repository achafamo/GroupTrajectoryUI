{-# Language
             TypeSynonymInstances,
             FlexibleInstances,
             TypeFamilies,
             GADTs

 #-}
module Data.Graph.Grouping.GroupingTree( Tree
                                       , CTree
                                       ) where

import Data.TimeInterval
import Data.Trajectory

import Data.Foldable hiding (elem)
import Data.Graph.Grouping.Types

import Prelude hiding (foldr,elem)


import qualified Data.Foldable as F

data Tree b a = Empty | Leaf a | Node (Tree b a) b (Tree b a)
                deriving (Show,Eq,Ord,Read)

-- | The main type we will be using to store sets of candidate groups
type CTree = Tree Time (Entity,Time)

--------------------------------------------------------------------------------
-- | Make sure a Tree can be used in an Entities

instance Foldable (Tree b) where
    foldr f z Empty        = z
    foldr f z (Leaf x)     = f x z
    foldr f z (Node l _ r) = foldr f (foldr f z r) l


instance CanStoreEntities (Tree Time) where

    fromList  = foldr ins Empty
        where
          ins x Empty = Leaf x
          ins x t     = Node (Leaf x) 0 t -- time is ignored here

    toList    = F.toList
    elem      = F.elem
    unions [] = Empty
    unions xs = F.foldr1 (\t1 t2 -> Node t1 0 t2) xs
    size      = length . F.toList


insert' i e Empty = Leaf (e,i)
insert' i e t     = Node (Leaf (e,i)) i t


--------------------------------------------------------------------------------
-- | The main use of CTrees

instance RepresentsCandGroups CTree where
    type CGElem CTree = CandidateGroup (Tree Time)

    singleton (Group es i) = foldr (insert (startTime i)) Empty es

    listGroups _ Empty        = []
    listGroups e (Leaf (x,s)) = [Group (Leaf x) (Interval (s,e))]
    listGroups e (Node l s r) = g :xs ++ ys
                                where
                                  xs@(gx:_) = listGroups e l
                                  ys@(gy:_) = listGroups e r
                                  comp      = Node (entities gx) e (entities gy)
                                  g         = Group comp (Interval (s,e))

    merge2 e t1 t2 = Node t1 e t2

    extract es Empty          = Empty
    extract es l@(Leaf (x,s)) = if x `elem` es then l else Empty
    extract es (Node l t r)   = let l' = extract es l
                                    r' = extract es r in
                                  case (l',r') of
                                    (Empty,_) -> r'
                                    (_,Empty) -> l'
                                    (_,_)     -> Node l' t r'

    ended e c = fst . ended' e c


data CGStatus = Elem | NotElem | Changed
              deriving (Show,Eq)

ended'                               :: CanStoreEntities c' =>
                                     Time -> c' Entity -> CTree -> ([CGElem CTree],CGStatus)
ended' _ _ Empty                     = ([], NotElem)
ended' _ c (Leaf (x,_)) | x `elem` c = ([], Elem)
ended' _ c (Leaf (x,_))              = ([], NotElem)
ended' e c n@(Node l s r) = let (gsl,statl) = ended' e c l
                                (gsr,statr) = ended' e c r
                                g           = head . listGroups e $ n
                                changed     = (g:gsl ++ gsr, Changed) in
                            case (statl,statr) of
                              (Changed, _)       -> changed
                              (_      , Changed) -> changed
                              (Elem   , Elem)    -> ([],Elem)
                              (NotElem, NotElem) -> ([],NotElem)
                              (_,_)              -> changed


-- | insert a new singleton set {e} into the tree
insert           :: Time -> Entity -> CTree -> CTree
insert i e Empty = Leaf (e,i)
insert i e t     = Node (Leaf (e,i)) i t


-- genSets            :: Tree -> [CandidateGroup]
-- genSets Empty      =  []
-- genSets (Leaf (x,t))   = [[x]]
-- genSets (Node l r)   = let xs@(x:_) = genSets l
--                          ys@(y:_) = genSets r in
--                      (x++y) : xs ++ ys
