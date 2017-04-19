{-# Language
               TypeSynonymInstances
             , FlexibleInstances
  #-}
module Data.Trajectory( Entity
                      , TP(..)
                      , getX
                      , getY
                      , getT
                      , Edge
                      , Trajectory(..)
                      , edges
                      , start
                      , stop
                      , startPoint
                      , endPoint
                      , numVertices
                      , scaleToRange
                      , pointAt
                      , dist
                      , subTrajectory
                      , maybeSubTrajectory
                      , synchronize
                      , synchronizeAndSparsen
                      , synchronizeToRegular
                      ) where

import Data.List
import Data.Maybe
import Data.TimeInterval

import Debug.Trace

import qualified Data.Set as S

type Entity = Int

newtype TP = TP (Double,Double,Time)
    deriving (Show,Eq,Ord)

instance HasTime TP where
    time = getT

unTP (TP t) = t

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

getX = fst3 . unTP
getY = snd3 . unTP
getT = thd3 . unTP


-- | time is ingored in the sense that we simply take the left arguments time value
instance Num TP where
    (TP (x,y,t)) + (TP (x',y',t')) = TP (x+x',y+y',t)

    negate (TP (x,y,t)) = TP (-x,-y,t)

    -- others don't make sense
    p * q         = undefined
    abs p         = undefined
    signum p      = undefined
    fromInteger p = undefined



type Edge = (TP,TP)

data Trajectory = Trajectory { tid      :: Entity
                             , vertices :: [TP]
                             }
                deriving (Show,Eq,Ord)

instance TimeInterval Trajectory where
    startTime = time . head . vertices
    endTime   = time . last . vertices


edges    :: Trajectory -> [Edge]
edges tr = let vs = vertices tr in zip vs (tail vs)


fromEdges              :: Entity -> [Edge] -> Trajectory
fromEdges x []         = error "trajectory without edges"
fromEdges x ((p,q):es) = Trajectory x (p:q:map snd es)


instance TimeInterval Edge where
    startTime (p,_) = time p
    endTime (_,q)   = time q



-- FIXME: stop takes O(\tau) time since vertices is a list
start,stop :: Trajectory -> Time
start = getT . startPoint
stop  = getT . endPoint

startPoint :: Trajectory -> TP
startPoint = head . vertices
endPoint :: Trajectory -> TP
endPoint = last . vertices

numVertices :: Trajectory -> Int
numVertices = length . vertices

-- | get the point on the line-segment between p and q at time t
-- (t in [0,1])
pointAt t p q | 0 <= t && t <= 1 = TP ( linear t (getX p) (getX q)
                                      , linear t (getY p) (getY q)
                                      , linear t (getT p) (getT q) )
pointAt t p q | otherwise = error $ "pointAt " ++ show (t,p,q)


-- | get the point on the line-segment between p and q at time t
-- (t in [time p, time q])
pointAt' t p q | time p <= t && t <= time q = let t' = (t - (time p)) / (time q - time p) in
                                              pointAt t' p q
pointAt' _ _ _ | otherwise = error "pointAt'"

-- | simple linear interpolation, assuming t in [0,1]
linear t x y = (1-t)*x + t*y


-- | scale time value t in the range [0,1] so it is in the range [pt,pq]
scaleToRange t p q = let (pt,qt) = (getT p, getT q) in pt + t*(qt - pt)



-- | euclidean distance between p and q
dist p q = let a = getX p - getX q
               b = getY p - getY q in sqrt (a*a + b*b)


select            :: TimeInterval i => i -> Edge -> Maybe Edge
select int e@(p,q) = case intersectsInterval e int of
                       Nothing               -> Nothing
                       Just (Interval (s,e)) -> Just (pointAt' s p q, pointAt' e p q)


-- | Get the subtrajectory of t on the time interval i.
-- precondition: i \subseteq [start t, stop t]
subTrajectory :: TimeInterval i => i -> Trajectory -> Trajectory
subTrajectory i = fromJust . maybeSubTrajectory i


maybeSubTrajectory       :: TimeInterval i => i -> Trajectory -> Maybe Trajectory
maybeSubTrajectory int t = case mapMaybe (select int) . edges $ t of
                             []  -> Nothing
                             es  -> Just $ fromEdges (tid t) es

synchronize :: [Trajectory] -> [Trajectory]
synchronize = synchronizeAndSparsen 1

synchronizeAndSparsen      :: Int -> [Trajectory] -> [Trajectory]
synchronizeAndSparsen k ts = let times = every k . gatherTimes $ ts in
                              map (addVerticesAt times) ts


-- | syncrhonize the trajectories, then subsample such that we retain only
-- rho% of the vertices, spaced regularly.
synchronizeToRegular        :: Double -> [Trajectory] -> [Trajectory]
synchronizeToRegular rho ts = let allTimes = gatherTimes ts
                                  start    = head allTimes
                                  end      = last allTimes
                                  k        = (rho/100) * (genericLength allTimes)
                                  t        = (end - start) / (k-1)
                                  times    = takeWhile (<= end) . iterate (+t) $ start in
                              map (addVerticesAt times) ts


every k  _ | k <= 0 = error "every: k <= 0"
every 1 xs          = xs -- speed up for the special case in which we do not sparsen
every _ []          = []
every k xs          = let (x:_,ys) = splitAt k xs in x:every k ys


gatherTimes :: [Trajectory] -> [Time]
--gatherTimes =  S.toAscList . S.fromList . map time . concatMap vertices
-- more efficient implementation:
gatherTimes =  S.toAscList . foldr insertAll S.empty
               where
                 insertAll t s = foldr (\v s' -> S.insert (time v) s') s $ vertices t


partitionTimes      :: Trajectory -> [Time] -> ([Time],[Time],[Time])
partitionTimes t ts = let (before,ts')   = span (<  (start t)) ts
                          (during,after) = span (<= (stop  t)) ts' in
                      (before,during,after)

-- | construct a trajectory based on tr that has vertices at all times in ts.
-- precondition:
-- - ts contains no duplicates
-- - ts is in ascending order
addVerticesAt                         :: [Time] -> Trajectory -> Trajectory
addVerticesAt ts tr@(Trajectory i vs) = Trajectory i (before' ++ during' ++ after')
    where
      (before,during,after) = partitionTimes tr ts
      before'               = map mkU before
      after'                = map mkV after
      during'               = addVerticesAt' during (edges tr)
      (TP (sx,sy,_))        = startPoint tr
      (TP (tx,ty,_))        = endPoint   tr
      mkU t                 = TP (sx,sy,t)
      mkV t                 = TP (tx,ty,t)
-- removed condition: since I don't think we need it
-- - gatherTimes vs \subseteq ts


-- | given a list ts of times at which there should be vertices, and the edges es
-- of a trajectory, generate the new list of all vertices. Let vs denote the existing
-- vertices, than we have as precondition:
--      - ts contains no duplicats
--      - ts is in ascending order
--      - for all t in ts : t in the range [time . first vs, time . last vs]
addVerticesAt'               :: [Time] -> [(TP,TP)] -> [TP]
addVerticesAt' [] []         = []
addVerticesAt' ts [(u,v)]    = map (\t -> pointAt' t u v) ts
addVerticesAt' ts ((u,v):es) = let (xs,ts')  = span (< (time v)) ts
                                   vs'       = map (\t -> pointAt' t u v) xs in
                               vs' ++ addVerticesAt' ts' es
-- removed condition: since I don't hink we need it
--      - gatherTimes [vs] \subseteq ts (i.e. all existing vertex times allready occur)



test s n = Trajectory n (map makeTp [s..n])
         where
           makeTp i = TP (fromIntegral i,0,fromIntegral i)



testTraj = test 0 15
