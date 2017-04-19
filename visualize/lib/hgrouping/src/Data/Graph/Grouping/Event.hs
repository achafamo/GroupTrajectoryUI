module Data.Graph.Grouping.Event( EKind(..)
                                , Event(..)
                                , eTime
                                , eKind
                                , unE
                                , events
                                ) where

import Data.List
import Data.Trajectory
import Data.TimeInterval

import Text.Printf

-- import Debug.Trace
-- traceShow x y = y
-- trace x y = y

data EKind = Join | Split
           deriving (Show,Eq,Ord)

newtype Event = Event (Entity,Entity,Time,EKind)
    deriving (Show,Eq)

unE (Event x) = x

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thd4 (a,b,c,d) = c
fth4 (a,b,c,d) = d

eTime = thd4 . unE
eKind = fth4 . unE


instance Ord Event where
    -- order on time and kind first
    (Event (a,b,c,d)) <= (Event (e,f,g,h)) = (c,d,a,b) <= (g,h,e,f)

-- | compute all event points for the trajectory edges |pq| and |rs|
-- corresponding to entities i and j, respectively. That is, compute the events
-- at which the distance between i and j is exactly eps.
events                     :: Double -> Entity -> Entity -> Edge -> Edge -> [Event]
events eps i j (p,q) (r,s) = let ets = evtTimes eps p q r s  in
                             case ets of
                               []      -> []
                               [t]     -> let k  = evtKind t (p,q) (r,s)
                                              t' = scaleToRange t  p q
--                                              t'' = trace ("time: "++ show t'') t''
                                          in
                                          [ Event (i, j, t', k)]
                               [t1,t2] -> [ Event (i, j, scaleToRange t1 p q, Join)
                                          , Event (i, j, scaleToRange t2 r s, Split)]


-- | computes the times t at which the distance between |pq| and |rs| is
--   exactly eps. The returned time values are in between 0 and 1
evtTimes eps p q r s = if discr < 0 then [] else
                           sort . filter (\t -> 0 <= t && t <= 1) $ times
                       where
                         a@(TP (ax,ay,_)) = q - p - s + r
                         b@(TP (bx,by,_)) = p - r
                         aA               = ax*ax + ay*ay
                         bB               = 2*(ax*bx + ay*by)
                         cC               = bx*bx + by*by - eps*eps
                         discr            = bB*bB - 4*aA*cC
                         times            = [ (-bB + sqrt discr) / (2*aA)
                                            , (-bB - sqrt discr) / (2*aA) ]
                         show' x = show x ++ "\n"



-- | Determine the kind of the one event point at time t
evtKind t (p,q) (r,s) = if d (t/2) < d t then Split else Join
    where
--      d t' = traceShow (t', d' t') (d' t')
      d t'= let x = (pointAt t' p q)
                y = (pointAt t' r s) in
             dist x y
             -- dist (trace ("X: "++show x) x) (trace ("Y:" ++ (show y)) y)
      -- If there is one time t at which |pq| and |rs| are at distance eps
      -- (which should be the distance d t) and d (t/2) is <= this means
      -- the segments diverge, hence we have a split event.
      msg st = printf "%f is a %s" t (show st)
