module Data.Graph.Ipe.DrawGraph( embed
                               , drawGraph
                               , drawVertex
                               , drawEdge ) where

import Data.Graph.Inductive.Graph

import Data.Maybe

import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Data.Geometry.Point
import Data.Geometry.Line

import Data.Geometry.Ipe.IpeTypes
import Data.Geometry.Ipe.IpeGeometryTypes
import Data.Geometry.Ipe.IGC(IGC)
import Data.Geometry.Ipe.IpeView
import Data.Geometry.Ipe.WriteIpeGeometry

import qualified Data.Map as M
import qualified Data.Geometry.Ipe.IGC as IGC


type IpePoint = IpePoint' Double
type IpePolyline = IpePolyline' Double

embed :: Graph gr => gr a b -> IO (gr (AttributeNode a) (AttributeEdge b))
embed = graphToGraph params
        where
          params = nonClusteredParams { isDirected       = True
                                      , globalAttributes = [
                                         GraphAttrs [ RankDir FromBottom
                                                    , Rank SourceRank
                                                    ]
                                                           ]
                                      }

drawGraph :: Graph gr => gr a b -> IO (ViewInstance Double)
drawGraph gr = do
  erg <- embed gr
  let igc = IGC.empty { IGC.points    = map (drawVertex . snd) . labNodes $ erg
                      , IGC.polyLines = map (drawEdge erg) . labEdges $ erg }
  return $ viewInstance [layer "graph" igc]

vertexPos :: AttributeNode a -> Point2' Double
vertexPos (ats,_) = Point2 (xCoord p, yCoord p)
    where
      [Pos (PointPos p)] = filter (sameAttribute (Pos {})) ats


drawVertex         :: AttributeNode a -> IpePoint
drawVertex = fromPoint . vertexPos

drawEdge           :: Graph gr => gr (AttributeNode a) b -> LEdge b -> IpePolyline
drawEdge g (u,v,_) = IpePolyline [LineSegment2 pu pv] attrs
    where
      pu    = vertexPos . fromJust . lab g $ u
      pv    = vertexPos . fromJust . lab g $ v
      attrs = M.fromList [("arrow","normal/normal")]
