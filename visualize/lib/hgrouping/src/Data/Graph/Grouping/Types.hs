{-# Language
             ExistentialQuantification,
             TypeFamilies,
             FlexibleInstances,
             FlexibleContexts,
             GADTs,
             KindSignatures,
             UndecidableInstances,
             DeriveDataTypeable
 #-}
module Data.Graph.Grouping.Types where

import Data.Trajectory
import Data.TimeInterval

import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.Data

import Text.Printf

import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.List as L

-----------------------------------------------------------------------------------
-- | The parameters of our algorithm

-- (disc size, min time span, min size)
data Parameters = Parameters { epsilon :: Double
                             , delta   :: Double
                             , m       :: Int }
                deriving (Show,Eq,Typeable,Data)

-- | Types Representing Entities and sets of Entities

class CanStoreEntities c where
    fromList :: [Entity] -> c Entity
    toList   :: c Entity -> [Entity]
    elem     :: Entity -> c Entity-> Bool
    unions   :: [c Entity] -> c Entity
    size     :: c Entity -> Int


instance CanStoreEntities Set where
    fromList = S.fromList
    toList   = S.toList
    elem     = S.member
    unions   = S.unions
    size     = S.size

instance CanStoreEntities [] where
    fromList = id
    toList   = L.nub
    elem     = L.elem
    unions   = concat
    size     = L.length

-------------------------------------------------------------------------------------
-- | Type Representing components, Groups and Candidate Groups

-- | A group is a set of entities with an associated time interval
data Group :: (* -> *) -> * where
  Group :: CanStoreEntities c => c Entity -> Interval -> Group c


instance Eq (c Entity) => Eq (Group c) where
    (Group xs i) == (Group ys j) = (xs,i) == (ys,j)

instance Show (c Entity) => Show (Group c) where
    show (Group xs i) = printf "Group %s %s" (show i) (show xs)

instance Ord (c Entity) => Ord (Group c) where
    -- compare stuff lexicographically, first on interval
    (Group xs i) <= (Group ys j) = (i,xs) <= (j,xs)

instance TimeInterval (Group c) where
    getInterval (Group _ i) = i


entities              :: Group c -> c Entity
entities (Group xs _) = xs

groupSize :: CanStoreEntities c => Group c -> Int
groupSize = size . entities


type CandidateGroup = Group

-----------------------------------------------------------------------------------
-- | Representing sets of (Candidate Groups)

class RepresentsCandGroups c where
    type CGElem c

    singleton    :: (F.Foldable c', CanStoreEntities c') => Group c'  -> c

    listGroups   :: Time ->  c  -> [CGElem c]
    merge2       :: Time ->  c  -> c -> c
    merge        :: Time -> [c] -> c
    merge t = F.foldr1 (merge2 t)
    extract      :: CanStoreEntities c' => c' Entity -> c -> c
    -- ended e c t is a list of groups that ends at time e, when the set of entities
    -- c is extracted from t
    ended        :: CanStoreEntities c' => Time -> c' Entity -> c -> [CGElem c]



-- -- | default implementation uses regular sets !
-- instance RepresentsCandGroups CandGroupCont where
--     fromList               = CGroups . S.fromList
--     toList (CGroups xs)    = toList xs
--     merge t xss            = undefined
--     extract t (CGroups xs) = CGroups . extract t xs
