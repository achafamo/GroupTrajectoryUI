import sys
import wx

from os.path import isfile


import itertools
import Image
import colorsys
import subprocess

import matplotlib.cm as cm
import matplotlib.colors as colors

import tempfile
import os

# try:
import time, random

from wx.lib.floatcanvas.Utilities import BBox

from objects     import *
from common      import *
from gui         import *

# import memdebug
# memdebug.start(8000)

################################################################################
### Settings

frameRate        = 15

sizeColorMap     = cm.autumn
durationColorMap = cm.cool

################################################################################

def encode(inPath, outPath):
    subprocess.call(ffmpeg(inPath,outPath),preexec_fn=os.setsid)

def ffmpeg(inpath = "pipe:", outpath = "/tmp/movie.mkv"):
    return [ "ffmpeg"
           , "-r", str(frameRate)
           , "-aspect", "1:1"
           , "-f", "image2pipe"
           , "-vcodec", "png"
           , "-i", inpath
           , outpath ]


def toWxColor(color):
    (r,g,b,a) = color
    def f(x):
        return int(x*255)
    return wx.Colour(f(r),f(g),f(b),a)

class World(DrawFrame):
    """
    """

    def setSelectorFunctions(self, edgesBy, tailsBy):
        mapping = { "size"     : self.colorBySize
                  , "duration" : self.colorByDuration
                  }

        self.edgesBy = mapping[edgesBy]
        self.tailsBy = mapping[tailsBy]


    def __init__(self, parent, wid, title, position, size,
                 grrs, settings):
        """

        Arguments:
        - `parent`:
        - `wid`:
        - `title`:
        - `position`:
        - `size`:
        - `entities`:
        - `groups`:
        """
        DrawFrame.__init__(self,parent, wid,title,position, size)
        self._parent = parent
        self._title = title
        self._position = position
        self._size = size
        self._grrs  = grrs
        self._entities = grrs[0]._entities
        self.settings = settings
        self._outPath = settings["outPath"]

        self._n   = len(self._entities)
        self._tau = self._entities[1].trajectory.length() if self._n > 0 else 0

        self.initColorByGroupSize()
        self.initColorByDuration()

        self.setSelectorFunctions(settings["edgesBy"],settings["tailsBy"])

        self.recordProcess = None

        self.CalcBoundingBox()

        return None



    def CalcBoundingBox(self):
        points = [x for x in itertools.chain.from_iterable(
            [e.trajectory.positions() for e in self._entities.values()])]
        self.bbox = BBox.fromPoints(points)


    def allGroups(self):
        return itertools.chain.from_iterable([r._groups for r in self._grrs])


    def initColorBy(self,keyF,low=None,high=None,colormap = cm.autumn):

        xs     = [keyF(g) for g in self.allGroups()]
        l = low  if low  else min(xs)
        h = high if high else max(xs)

        norm   = colors.Normalize(vmin=l,vmax=h,clip=True)

        def colorF(g):
            return toWxColor(colormap(norm(keyF(g))))

        return  dict([(g,colorF(g)) for g in self.allGroups()])

    def initColorByGroupSize(self):
        st = self.settings
        self._colorsBySize = self.initColorBy(lambda g: g.size(),
                                              st["minSize"],st["maxSize"],
                                              sizeColorMap)


    def initColorByDuration(self):
        st = self.settings
        self._colorsByDuration = self.initColorBy(lambda g: g.duration(),
                                                  st["minDuration"], st["maxDuration"],
                                                  durationColorMap)


    def initColorByGroupingResult(self):
        # create a mapping between the groups and the grouping results
        # they occur in.
        self._groupingMapping = dict([(g,r) for r in self._grrs for g in r._groups])
        colors = ["red","green","blue","yellow","purple"]
        self._colorsByGroupingResult = dict(zip(self._grrs, colors))


    # lookup the color of this group
    def colorBySize(self, group):
        return self._colorsBySize[group]

    # lookup the color of this group
    def colorByDuration(self, group):
        return self._colorsByDuration[group]

    def colorByGroupingResult(self, group):
        return self._colorsByGroupingResult[self._groupingMapping[group]]

    def recordWorld(self):

        def skip(c):
            pass

        def recordFrame(c):
            # c.SaveAsImage("/tmp/test/{0:05d}.png".format(self.TimeStep))
            wximg = c._Buffer.ConvertToImage()
            img = Image.new( 'RGB', (wximg.GetWidth(), wximg.GetHeight()) )
            img.fromstring(wximg.GetData())
            img.save(self.recordProcess,"png")
            # self.recordProcess.flush()
            # img.save(self.recordProcess.stdin,"png")

        if self._outPath:
            self.recordProcess = tempfile.NamedTemporaryFile('wb')
            self.playWorld(recordFrame)
            encode(self.recordProcess.name, self._outPath)
            # self.recordProcess = subprocess.Popen(ffmpeg(self._outPath),
            #                                       stdin=subprocess.PIPE)
        else:
            self.playWorld(skip)



    def playWorld(self,recordf):
        for (i,t) in zip(range(0,self._tau),self._entities[1].trajectory.times):
            self.Canvas.InitAll()
            self.drawWorldAtTime(i,t)
            self.Canvas.ZoomToBB(self.bbox)
            recordf(self.Canvas)
#            print h.heap().byrcs


    def drawWorldAtTime(self, i, t):
        """

        Arguments:
        - `t`:
        """

        # draw the groups
        for r in self._grrs:
            for g in [g for g in r._groups if g.isActive(t)]:
                self.drawGroup(g,t,r._epsilon,r._delta,r._m)

        # draw the entities

        for e in self._entities.values():
            self.drawEntity(e, i, t)

    def drawTrajectory(self,tr,color = "grey"):
        self.Canvas.AddLine(tr.positions(), LineColor = color)


    def drawEntity(self, e, i, t, color = 'white'):
        """

        Arguments:
        - `e`:
        - `i`: index such that e.times[i] = t
        - `t`:
        """

        tp = t                     if i == 0 else e.trajectory.times[i-1]
        tq = e.trajectory.times[1] if i == 0 else t

        # print("tp :  " + str(tp) )
        # print("t  :  "  + str(t) )
        # print("tq :  " + str(tq) )
        # print("next: " + str(e.nextT(tp)))
        # print(e.nextT(tp) == t)

        p = e.position(tp)
        q = e.position(tq)
        (x,y) = minp(q,p)

        # show where the entity was a few timesteps ago using a line
        self.drawEntityTail(e,t,"darkgrey")

        # we draw the arrow after the tails to keep them on top
        o = EntityArrow(e.position(t),
                        color = color, size = 8,
                        direction = atan2(y,x))
        self.Canvas.AddObject(o)



    def drawGroup(self, g, t, eps, delta, m):
        """

        Arguments:
        - `g`:
        - `t`:
        - `eps`:
        """
        def diste(e1,e2):
            return dist(e1.position(t),e2.position(t))

        # first draw the tails, then the group edges
        self.drawGroupTail(g,t,self.tailsBy)

        edgeColor = self.edgesBy(g)

        for e1 in g.members:
            for e2 in [e for e in g.members if e != e1 and diste(e1,e) < 2*eps]:
                points = [e1.position(t), e2.position(t)]
                self.Canvas.AddLine(points,
                                    LineWidth = 3, LineColor = edgeColor)



    def drawEntityTail(self, e, t, color):
        if not (t > 0 and self.settings["entityTailLength"] > 0):
            return
        s = max(0,t-self.settings["entityTailLength"])
        self.drawTrajectory(e.trajectory.subtrajectory(s,t),color)

    def drawGroupTail(self, g, t, colorF):
        sp = int(round(g.start))
        if not (t > sp and self.settings["groupTailLength"] > 0):
            return
        s = max(sp,t-self.settings["groupTailLength"])

        color = colorF(g)
        for e in g.members:
            self.drawTrajectory(e.trajectory.subtrajectory(s,t),color)



class DemoApp(wx.App):
    WORLDWIDTH = 256
    WORLDHEIGHT = 256


    def __init__(self, grrs, settings, *args, **kwargs):
        self._grrs     = grrs
        self._settings = settings
        wx.App.__init__(self, *args, **kwargs)

    def OnInit(self):
        # wx.InitAllImageHandlers()
        frame = World(None, -1, "Trajectory Grouping Structure",wx.DefaultPosition,
                      (4*self.WORLDWIDTH,4*self.WORLDHEIGHT),
                      self._grrs, self._settings)

        self.SetTopWindow(frame)
        frame.Show()
        frame.recordWorld()
        sys.exit(0)



def main(args):
    """
    """
    def get(l,i,d,f=lambda x:x):
        return f(l[i]) if i < len(l) else d

    settings = { "inFile"           : get(args,1,"/tmp/test.txt")
               , "outPath"          : get(args,2,"/tmp/movie.mkv")
               , "entityTailLength" : get(args,3,0,float)        # length of the entity tails
               , "groupTailLength"  : get(args,4,40,float)       # length of the group  tails (use float("infinity")) for inf.
               , "minSize"          : get(args,5,None,int)
               , "maxSize"          : get(args,6,None,int)
               , "minDuration"      : get(args,7,None,float)
               , "maxDuration"      : get(args,8,None,float)
               , "edgesBy"          : get(args,9,"size")
               , "tailsBy"          : get(args,10,"duration")
               }



    inFile = settings["inFile"]
    grrs = [fromFile(inFile)] if isfile(inFile) else fromDir(inFile)

    # print h.heap().byrcs

    app = DemoApp(grrs, settings)
    app.MainLoop()



if __name__ == '__main__':
    main(sys.argv)
