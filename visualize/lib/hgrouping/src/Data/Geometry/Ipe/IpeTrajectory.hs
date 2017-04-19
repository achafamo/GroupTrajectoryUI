module Data.Geometry.Ipe.IpeTrajectory( -- ^ read trajectories from ipe
                                        readTrajectories
                                      , toTrajectory
                                      -- ^ output trajectories to ipe
                                      , drawTrajectory
                                      , drawTrajectory'
                                      , defaultTrajAttrs
                                      , trajectoriesView
                                      , trajectoriesLayer
                                      ) where

import Data.Trajectory

import Data.Geometry.Point
import Data.Geometry.Geometry
import Data.Geometry.Line

import Data.Geometry.Ipe.IpeTypes
import Data.Geometry.Ipe.IpeGeometryTypes
import Data.Geometry.Ipe.IpeGeometry
--import Data.Geometry.Ipe.ReadIpeGeometry
import Data.Geometry.Ipe.IGC(IGC)
import Data.Geometry.Ipe.IpeView



import qualified Data.Map as M
import qualified Data.Geometry.Ipe.IGC as IGC
import qualified Data.Graph.Grouping.Types as G


type Point2 = Point2' Double
type IpePolyline = IpePolyline' Double

--------------------------------------------------------------------------------
-- | Stuff to read trajectories from ipe stuff

readTrajectories :: IGC Double -> [Trajectory]
readTrajectories = map toTrajectory . zip [1..] . IGC.polyLines

toTPs    :: [Point2' Double] -> [TP]
toTPs ts = map (\(t,Point2 (x,y)) -> TP (x,y,t)) $ zip [0..] ts

toTrajectory :: (Entity,IpePolyline) -> Trajectory
toTrajectory (i,pl) = Trajectory i (toTPs . points $ pl)


--------------------------------------------------------------------------------
-- | Stuff to convert trajectories to ipe stuff

drawTrajectory :: Trajectory -> IpePolyline
drawTrajectory = drawTrajectory' defaultTrajAttrs

drawTrajectory'         :: AMap -> Trajectory -> IpePolyline
drawTrajectory' attrs t =  IpePolyline (map seg . edges $ t) attrs
    where
      seg (p,q) = LineSegment2 (toPoint p) (toPoint q)

toPoint :: TP -> Point2
toPoint (TP (x,y,_)) = Point2 (x,y)


-- | default attributes to display a trajectory with
defaultTrajAttrs :: AMap
defaultTrajAttrs = M.fromList [ ("stroke", "black")
                              , ("pen"   , "1")
                              , ("arrow" , "normal/normal")
                              ]

-- | create a view with one layer lName displaying all trajectories
trajectoriesView           :: String -> [Trajectory] -> ViewInstance Double
trajectoriesView lName tss =  viewInstance [trajectoriesLayer lName tss]

-- | create a layer displaying all trajectories
trajectoriesLayer           :: String -> [Trajectory] -> Layer Double
trajectoriesLayer lName tss =   layer lName igc
    where
      igc = IGC.empty { IGC.polyLines = map drawTrajectory tss }
