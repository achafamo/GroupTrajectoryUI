module Data.TimeInterval( Time
                        , Interval(..)
                        , HasTime(..)
                        , TimeInterval(..)
                        ) where

import Text.Printf

----------------------------------------------------------------------------------
-- | Types that have to do with Time

type Time = Double
newtype Interval = Interval (Time,Time)
                   deriving (Eq,Ord)

instance Show Interval where
    show (Interval (s,e)) = printf "[%f,%f]" s e

unI              :: Interval -> (Time,Time)
unI (Interval x) = x

class HasTime t where
    time :: t -> Time

class TimeInterval ti where
    startTime :: ti -> Time
    startTime = fst . unI . getInterval
    endTime   :: ti -> Time
    endTime   = snd . unI . getInterval

    getInterval   :: ti -> Interval
    getInterval x = Interval (startTime x, endTime x)

    duration   :: ti -> Time
    duration i =  endTime i - startTime i

    inInterval       :: HasTime o => o -> ti -> Bool
    inInterval o int =  let t = time o in
                        startTime int <= t && t <= endTime int


    intersectsInterval :: TimeInterval ti' => ti -> ti' -> Maybe Interval
    intersectsInterval x y = getInterval x `intersectI` getInterval y



intersectI :: Interval -> Interval -> Maybe Interval
intersectI (Interval (s1,e1)) (Interval (s2,e2)) = let s = max s1 s2
                                                       e = min e1 e2 in
                                                   if s <= e then
                                                       Just $ Interval (s,e)
                                                   else Nothing


instance TimeInterval Interval where
    startTime = fst . unI
    endTime   = snd . unI
