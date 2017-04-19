{-# Language
          DeriveDataTypeable
  #-}
module Main where

--import System.Environment
import System.Console.CmdArgs
import System.FilePath.Posix

import Control.Monad


import Data.Ord
import Data.List
import Data.TimeInterval
import Data.Trajectory
import Data.Graph.Grouping.GroupingStructure
import Data.Graph.Grouping.Types
import Data.Graph.Grouping.BuildReebGraph
import Data.Graph.Grouping.ReebGraph
import Data.Graph.Grouping.GroupingTree

import Data.GroupingIO

import Text.Printf
import Debug.Trace

import qualified Data.Set as S
import qualified Data.Graph.Grouping.Types as GT

--------------------------------------------------------------------------------
-- | Stuff on command line arguments


data GRMode = Compute | Vis
              deriving (Eq,Show,Data,Typeable)

data CliArgs = CliArgs { inFile  :: FilePath
                       , outFile :: FilePath
                       , mode    :: GRMode
                       , params  :: Parameters
                       }
               deriving (Show,Data,Typeable)

instance Default GRMode where
    def = Compute

instance Default Parameters where
    def = Parameters { epsilon  = 10
                     , delta    = 3.5
                     , m        = 3
                     }


defaultArgs = CliArgs { inFile   = def             &= help "input file"
                      , outFile  = "/tmp/out.json" &= help "output file"
                      , mode     = def
                      , params   = def
                      }

ipeArgs     = defaultArgs { inFile  = "../../input/ipe/acmgis.ipe"
                          , outFile = "/tmp/out.ipe" }

netLogoArgs = defaultArgs { inFile  = "../../input/netlogo/trajectories_400_818.netlogo"
                          , mode    = Compute
                          , params  = Parameters { epsilon = 7.5
                                                 , delta   = 40
                                                 , m       = 4
                                                 }
                          }

starkeyArgs = defaultArgs { inFile  = "../../input/starkey/starkey_deer.starkey"
                          , outFile = "/tmp/starkey_deer.netlogo"
                          , mode    = Compute
                          , params  = Parameters { epsilon = 10
                                                 , delta   = 500
                                                 , m       = 4
                                                 }
                          }

selectIO       :: FilePath -> GroupingIO
selectIO path = let s = tail . takeExtension $ path in
                case s of
                  "ipe"      -> ipeIO
                  "netlogo"  -> netLogoIO
                  "starkey"  -> starkeyIO
                  "json"     -> jsonIO
                  _          -> jsonIO

--------------------------------------------------------------------------------
-- | Main methods

-- | args: [input,output,eps,delta,m]
main :: IO ()
main = mainWith =<< cmdArgs defaultArgs

type T = Data.Graph.Grouping.GroupingTree.Tree Double

mainWith                      :: CliArgs -> IO ()
mainWith (CliArgs iP oP m ps) = let inIO  = selectIO iP
                                    outIO = selectIO oP in
                                do
                                  gd  <- runFromFile inIO iP :: IO (GroupingData T)
                                  let res = runMode m ps gd
                                  report res
                                  runToFile outIO res oP

runMode              :: GRMode -> Parameters -> GroupingData T -> GroupingData T
runMode Compute ps = compute ps
runMode Vis     _  = visualize

compute      :: Parameters -> GroupingData T -> GroupingData T
compute ps@(Parameters eps delta m) (GroupingData ts _ _) = GroupingData ts grs ps
    where
      rg'         = buildReebGraph eps ts
      rg         = traceShow rg' rg'
      grs'       = groups' ps rg
      grs        = reverse . sortBy (comparing groupSize) $ grs'


-- | depending on what we exactly want to visualize we can also choose to retain only the active ones or so.

visualize = id

visualizeActive (GroupingData ts grs ps) = GroupingData ats grs ps
    where
      ats        = activeTrajectories grs ts



report (GroupingData ts grs (Parameters eps delta m)) = do
  putStrLn "=========== Grouping Data ============="
  putStrLn "---- Parameters: "
  putStrLn $ printf "epsilon = %f" eps
  putStrLn $ printf "delta   = %f" delta
  putStrLn $ printf "m       = %d" m
  -- print "- Trajectories: "
  -- mapM print ts
  putStrLn "---- Groups: "
  mapM (print . reportGroup) grs
  putStrLn "======================================="


activeTrajectories     :: CanStoreEntities c => [Group c] -> [Trajectory] -> [Trajectory]
activeTrajectories grs = filter (\t -> tid t `S.member` actEnts)
    where
      actEnts = S.unions . map (S.fromList . GT.toList . entities) $ grs

reportTrajectory                       :: Trajectory -> String
reportTrajectory (Trajectory ti (v:_)) = printf "%d starts at %s" ti (show v)

reportGroup              :: Group a -> String
reportGroup (Group xs i) = let ents = init . tail . show . toList $ xs in
    printf "Group {%s} on [%.2f,%.2f]" ents (startTime i) (endTime i)
