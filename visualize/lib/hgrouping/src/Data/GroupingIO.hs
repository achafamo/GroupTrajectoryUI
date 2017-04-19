{-# Language TemplateHaskell,
             FlexibleInstances,
             OverloadedStrings,
             ExistentialQuantification,
             RankNTypes
 #-}
module Data.GroupingIO( GroupingData(..)
                      , GroupingIO(..)
                      , netLogoIO
                      , starkeyIO
                      , ipeIO
                      , jsonIO
                      , writeOutput
                      ) where

import Data.Trajectory
import Data.TimeInterval
import Data.Graph.Grouping.Types
import Data.List
import Data.Maybe
import Data.List.Split


import Data.Geometry.Ipe.IpeTypes(IpeDrawing,emptyDrawing)
import Data.Geometry.Ipe.Pickle
import Data.Geometry.Ipe.IpeGeometry
import Data.Geometry.Ipe.TrajectoryGroup
import Data.Geometry.Ipe.WriteIpeGeometry

import Text.Printf

import Data.Graph.Grouping.ReebGraph

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH

import Debug.Trace

import qualified Data.Geometry.Ipe.IpeTrajectory as I
import qualified Data.Graph.Grouping.Types as GT
import qualified Data.ByteString.Lazy as B

--------------------------------------------------------------------------------
-- | General definitions

-- | A result for pickling/unpickling the computed grouping structure. When reading
-- input it is assumed the trajectories have all vertices at exactly the same times.
data GroupingData c = GroupingData { trajectories :: [Trajectory]
                                   , groups       :: [Group c]
                                   , parameters   :: Parameters
                                   }


data GroupingIO = GroupingIO {
        runFromFile :: CanStoreEntities c => FilePath -> IO (GroupingData c)
      , runToFile   :: CanStoreEntities c => GroupingData c -> FilePath -> IO ()
                             }





-- | Expresses that a type supports grouping IO. Minimal implementation consists
-- of fromFile and toFile
class CanDoGroupingIO t where
    fromFile :: CanStoreEntities c => t -> IO (GroupingData c)

    toFile                             :: CanStoreEntities c => GroupingData c -> t -> IO ()

    readTrajectories   :: t -> IO [Trajectory]
    readTrajectories x = let r = fromFile x :: IO (GroupingData []) in trajectories <$> r

--------------------------------------------------------------------------------
-- | Stuff to read and write in a format we can/read write in Netlogo

netLogoIO = GroupingIO (fromFile . NetLogoIO) (\c -> toFile c . NetLogoIO)

newtype NetLogoIO = NetLogoIO FilePath
                      deriving (Show,Read,Eq)

instance CanDoGroupingIO NetLogoIO where
    fromFile (NetLogoIO path) = do
      (ns:taus:rest) <- fmap lines . readFile $ path
      let n   = read ns :: Int
          tau = read taus :: Int
          tss = splitEvery tau rest
          ts  = map toTrajectoryNL $ zip [1..] tss
      return $ GroupingData ts undefined undefined

    toFile (GroupingData ts gs (Parameters e d m)) (NetLogoIO path) =
        writeOutput path ts gs e d m

toTrajectoryNL           :: (Entity, [String]) -> Trajectory
toTrajectoryNL (i,lines) = Trajectory i (map toTPsNL $ zip lines [0..])


toTPsNL       :: (String,Time) -> TP
toTPsNL (s,t) = TP (x,y,t)
    where
      [x,y] = map read . words . clean $ s
      clean = init . tail

writeOutput :: CanStoreEntities c =>
                FilePath -> [Trajectory] -> [Group c] -> Double -> Double -> Int -> IO ()
writeOutput path ts grs eps delta m =
    writeFile path (writeOutput' ts grs eps delta m)

writeOutput' :: CanStoreEntities c =>
                 [Trajectory] -> [Group c] -> Double -> Double -> Int -> String
writeOutput' ts grs eps delta m = let n    = length ts
                                      tau  = length . vertices . head $ ts
                                      numg = length grs
                                      tss  = intercalate "\n" . map writeTrajectory $ ts
                                      grss = intercalate "\n" . map writeGroup $ grs in
--                       printf "%d %d %d\n%s\n%s\n" n tau m tss grss
-- Printf is broken, see bug 2583 (http://hackage.haskell.org/trac/ghc/ticket/2583)
-- so we have to do it slightly differently
                       let x = printf "%d %d %d %f %f %d\n" n tau numg eps delta m in
                               x ++ tss ++ "\n" ++ grss ++ "\n"


writeTrajectory = intercalate "\n" . map writeTP . vertices

writeTP (TP (t,x,y)) = printf "%f %f %f" t x y

writeGroup g = let es = intercalate " " . map show . GT.toList . entities $ g
                   s  = startTime . getInterval $ g
                   e  = endTime   . getInterval $ g
                   n  = groupSize g in
               printf "%d %f %f\n%s" n s e es

--------------------------------------------------------------------------------
-- | Stuff to read trajectories from an input ipe file

ipeIO = GroupingIO (fromFile . IpeIO) (\c -> toFile c . IpeIO)

newtype IpeIO = IpeIO FilePath
                      deriving (Show,Read,Eq)

instance CanDoGroupingIO IpeIO where
    fromFile (IpeIO path) = do
      d  <- loadDrawing path :: IO (IpeDrawing Double)
      let ts = runSimple' I.readTrajectories d
      return $ GroupingData ts undefined undefined
    toFile (GroupingData ts gr _) (IpeIO path) = storeDrawing d path
        where
          d = foldr addViewToDrawing emptyDrawing [ I.trajectoriesView "trajectories" ts
                                                  , drawGroups' gr ts
                                                  ]

--------------------------------------------------------------------------------
-- | Stuff to read trajectories from an input ipe file

starkeyIO = GroupingIO (fromFile . StarkeyIO) (\c -> toFile c . StarkeyIO)


newtype StarkeyIO = StarkeyIO FilePath
                      deriving (Show,Read,Eq)

instance CanDoGroupingIO StarkeyIO where
    fromFile (StarkeyIO path) = do
      (ns:tauss:rest) <- fmap lines . readFile $ path
      let n        = read ns :: Int
          taus     = map read . splitOn " " $ tauss
          tss      = splitPlaces taus rest
          ts       = map starkeyTrajectory $ zip [1..] tss
          -- syncedTs = synchronizeAndSparsen 10 ts
          syncedTs = synchronizeToRegular 10 ts
      print (n,numVertices . head $ syncedTs)
     -- mapM_ print syncedTs
      -- let d' = addViewToDrawing (I.trajectoriesView "trajectories" ts) emptyDrawing
      -- storeDrawing d' "/tmp/in.ipe"
      -- let d = addViewToDrawing (I.trajectoriesView "trajectories" syncedTs) emptyDrawing
      -- storeDrawing d "/tmp/synced.ipe"
      return $ GroupingData syncedTs undefined undefined

    toFile = undefined

starkeyTP :: String -> TP
starkeyTP s = let [x,y,t] = map read . splitOn " " $ s in
              TP (x,y,t)

starkeyTrajectory        :: (Entity,[String]) -> Trajectory
starkeyTrajectory (i,ss) =  Trajectory i (map starkeyTP ss)





--------------------------------------------------------------------------------
-- | Stuff to read trajectories from an JSON file

$(deriveFromJSON id ''TP)
$(deriveFromJSON id ''Trajectory)
$(deriveFromJSON id ''Interval)
$(deriveFromJSON id ''Parameters)


instance CanStoreEntities c => FromJSON (Group c) where
    parseJSON (Object v) = Group                         <$>
                           (fromList <$> v .: "group")   <*>
                                         v .: "interval"
    parseJSON _          = empty

instance CanStoreEntities c => FromJSON (GroupingData c) where
    parseJSON (Object v) = GroupingData        <$>
                           v .: "trajectories" <*>
                           v .: "groups"       <*>
                           v .: "parameters"
    parseJSON _          = empty

newtype JSONIO = JSONIO FilePath
                      deriving (Show,Read,Eq)

instance CanDoGroupingIO JSONIO where
    fromFile  (JSONIO path) = fromJust . decode <$> B.readFile path
    toFile gd (JSONIO path) = B.writeFile path . encode $ gd


jsonIO = GroupingIO (fromFile . JSONIO) (\c -> toFile c . JSONIO)

--------------------------------------------------------------------------------
-- | and what we need to Write to JSON

$(deriveToJSON id ''TP)
$(deriveToJSON id ''Trajectory)
$(deriveToJSON id ''Interval)
$(deriveToJSON id ''Parameters)

instance CanStoreEntities c => ToJSON (Group c) where
    toJSON (Group xs i) = object [ "group"    .= toList xs
                                 , "interval" .= i ]

instance CanStoreEntities c => ToJSON (GroupingData c) where
    toJSON (GroupingData ts gs ps) = object [ "trajectories" .= ts
                                            , "groups"       .= gs
                                            , "parameters"   .= ps
                                            ]
