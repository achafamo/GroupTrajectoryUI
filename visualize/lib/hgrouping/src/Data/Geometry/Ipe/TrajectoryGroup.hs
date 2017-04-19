module Data.Geometry.Ipe.TrajectoryGroup( drawGroups
                                        , drawGroups'
                                        , drawGroup ) where


import Data.Maybe

import Data.Graph.Grouping.Types

import Data.Geometry.Point
import Data.Geometry.Line

import Data.Geometry.Ipe.IpeTypes hiding (Group)
import Data.Geometry.Ipe.IpeGeometryTypes
import Data.Geometry.Ipe.IGC(IGC)
import Data.Geometry.Ipe.IpeView
import Data.Geometry.Ipe.WriteIpeGeometry

import Data.Geometry.Ipe.IpeTrajectory

import Data.Trajectory

import Text.Printf

import qualified Data.Map as M
import qualified Data.Geometry.Ipe.IGC as IGC
import qualified Data.Graph.Grouping.Types as G

type Point2 = Point2' Double
type IpePolyline = IpePolyline' Double


type EGroup c = (Group c, Int)


-- color Int ::  -> String
-- colors !! (x `mod` length colors)

onSize g = printf "groupsize%d" (groupSize g)

-- color g = printf "groupsize%d" (groupSize g)
-- pen g = let x = groupSize g in
--         show $ x*x

defaultGroupAttrs       :: CanStoreEntities c => EGroup c -> AMap
defaultGroupAttrs (g,i) =   M.fromList [ ("stroke", onSize g)
                                       , ("pen", onSize g)
                                       , ("opacity", onSize g)
                                       ]

drawGroups :: CanStoreEntities c =>
              [Group c] -> [Trajectory] -> IpeDrawing Double -> IpeDrawing Double
drawGroups grs ts = addViewToDrawing (drawGroups' grs ts)


drawGroups' :: CanStoreEntities c => [Group c] -> [Trajectory] -> ViewInstance Double
drawGroups' grs ts = viewInstance . map mkLayer $ zip grs [1..]
    where
      mkLayer       :: CanStoreEntities c => EGroup c -> Layer Double
      mkLayer (g,i) = layer (printf "group_%d" i) (drawGroup (g,i) ts)

drawGroup :: CanStoreEntities c => EGroup c -> [Trajectory] -> IGC Double
drawGroup = drawGroup' (M.fromList [])

drawGroup' :: CanStoreEntities c => AMap -> EGroup c -> [Trajectory] -> IGC Double
drawGroup' attrs g@((Group es int),i) ts = IGC.empty { IGC.polyLines =  polys }
    where
      -- the subtrajectories that are part of the group
      ts'       = mapMaybe (maybeSubTrajectory int) . filter inGroup $ ts
      inGroup t = tid t `G.elem` es
      -- the polylines representing the group
      polys     = map (drawTrajectory' attrs') ts'
      attrs'    = attrs `M.union` (defaultGroupAttrs g)



colors = [ "darkorange", "gold" , "antiquewhite2" , "bisque1" ]


-- colors = [ "red"
--          , "green"
--          , "blue"
--          ,  "orange"
--          ,  "purple"
--          ,  "turquoise"
--          ,  "brown"
--          ,  "navy"
--          ,  "yellow"
--          ,  "pink"
--          ,  "seagreen"
--          ,  "violet"
--          ,  "darkblue"
--          ,  "darkcyan"
--          ,  "darkgray"
--          ,  "darkgreen"
--          ,  "darkmagenta"
--          ,  "darkorange"
--          ,  "darkred"
--          ,  "aliceblue"
--          ,  "antiquewhite"
--          ,  "antiquewhite1"
--          ,  "antiquewhite2"
--          ,  "antiquewhite3"
--          ,  "antiquewhite4"
--          ,  "gold"
--          ,  "aquamarine"
--          ,  "aquamarine1"
--          ,  "aquamarine2"
--          ,  "aquamarine3"
--          ,  "aquamarine4"
--          ,  "azure"
--          ,  "azure1"
--          ,  "azure2"
--          ,  "azure3"
--          ,  "azure4"
--          ,  "beige"
--          ,  "bisque"
--          ,  "bisque1"
--          ,  "bisque2"
--          ,  "bisque3"
--          ,  "bisque4"
--          ,  "blanchedalmond"]
