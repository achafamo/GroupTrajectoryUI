from math import cos,sin,atan2,pi

import wx

from wx.lib.floatcanvas.Utilities import BBox
from wx.lib.floatcanvas import NavCanvas, FloatCanvas
import wx.lib.colourdb

import numpy as N
import scipy as S


from objects import *
from common  import *


class EntityArrow(FloatCanvas.SquarePoint):
    """
    """

    def __init__(self, point,
                 color = "Black", size = 4,
                 inForeground = False, direction = 0):
        """

        Arguments:
        - `point`:
        - `color`:
        - `size`:
        - `inForeground`:
        - `direction`:
        """
        FloatCanvas.SquarePoint.__init__(self, point, color, size, inForeground)
        self._direction = direction
        self._color = color


    def _Draw(self, dc, worldToPixel, scaleWorldToPixel, hTdc = None):
        size = self.Size
        dc.SetPen(self.Pen)
        dc.SetBrush(self.Brush)
        xc,yc = worldToPixel(self.XY)



        r = size / 2
        # theta = radians(-self._direction + 180) # clocwise rotation, vertical = 0
        theta = self._direction + pi/2

        basepolygon = N.array([(0,r),(r,-r),(0,-r/2),(-r,-r)])

        matrix = N.array([ [ cos(theta),   -sin(theta) ] ,
                           [ sin(theta),   cos(theta)] ]
                         , N.float)

        # rotate and translate
        polygon = S.dot(basepolygon,matrix)+N.array([xc,yc])

        if self.Size <= 1:
            dc.DrawPoint(xc, yc)
        else:
            x = xc - size/2.0
            y = yc - size/2.0
            dc.SetBrush(self.Brush)
            dc.DrawPolygon(polygon)
        if hTdc and self.HitAble:
            hTdc.SetPen(self.HitPen)
            if self.Size <= 1:
                htdc.DrawPoint(xc, xc)
            else:
                hTdc.SetBrush(self.HitBrush)
                hTdc.DrawPolygon(polygon)


class DrawFrame(wx.Frame):

    """
    A frame used for the FloatCanvas Demo

    """

    def __init__(self,parent, id,title,position,size):
        wx.Frame.__init__(self,parent, id,title,position,(1280,1280))

            ## Set up the MenuBar
        MenuBar = wx.MenuBar()

        file_menu = wx.Menu()
        item = file_menu.Append(-1, "&Close","Close this frame")
        self.Bind(wx.EVT_MENU, self.OnQuit, item)

        item = file_menu.Append(-1, "&SavePNG","Save the current image as a PNG")
        self.Bind(wx.EVT_MENU, self.OnSavePNG, item)
        MenuBar.Append(file_menu, "&File")

        draw_menu = wx.Menu()


        self.SetMenuBar(MenuBar)

        self.CreateStatusBar()


        # Add the Canvas
        NC = NavCanvas.NavCanvas(self,
                                 Debug = 0,
                                 size = size,
                                 BackgroundColor = "BLACK")

        self.Canvas = NC.Canvas # reference the contained FloatCanvas



        self.MsgWindow = wx.TextCtrl(self, wx.ID_ANY,
                                     "Look Here for output from events\n",
                                     style = (wx.TE_MULTILINE |
                                              wx.TE_READONLY |
                                              wx.SUNKEN_BORDER)
                                     )

            ##Create a sizer to manage the Canvas and message window
        MainSizer = wx.BoxSizer(wx.VERTICAL)
        MainSizer.Add(NC, 4, wx.EXPAND)
        MainSizer.Add(self.MsgWindow, 1, wx.EXPAND | wx.ALL, 5)

        self.SetSizer(MainSizer)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

        # self.Canvas.Bind(FloatCanvas.EVT_MOTION, self.OnMove)
        # self.Canvas.Bind(FloatCanvas.EVT_MOUSEWHEEL, self.OnWheel)

        self.EventsAreBound = False

            ## getting all the colors for random objects
        wx.lib.colourdb.updateColourDB()
        self.colors = wx.lib.colourdb.getColourList()


        return None


    def Log(self, text):
        self.MsgWindow.AppendText(text)
        if not text[-1] == "\n":
            self.MsgWindow.AppendText("\n")


    def clearCanvas(self):
        self.Canvas.ClearAll()
        # self.Canvas._DrawList = []
        # self.Canvas._ForeDrawList = []
        # self.Canvas.MakeNewBuffers()

    def PrintCoords(self,event):
        self.Log("coords are: %s"%(event.Coords,))
        self.Log("pixel coords are: %s\n"%(event.GetPosition(),))

    def OnSavePNG(self, event=None):
        import os
        dlg = wx.FileDialog(
            self, message="Save file as ...", defaultDir=os.getcwd(),
            defaultFile="", wildcard="*.png", style=wx.SAVE
            )
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            if not(path[-4:].lower() == ".png"):
                path = path+".png"
                self.Canvas.SaveAsImage(path)


    def ZoomToFit(self,event):
        self.Canvas.ZoomToBB()

    def OnQuit(self,event):
        self.Close(True)

    def OnCloseWindow(self, event):
        self.Destroy()


    def ShowFrame(self):
        Object = self.MovingObject
        Range = self.Range
        if  self.TimeStep < self.NumTimeSteps:
            x,y = Object.XY
            if x > Range[1] or x < Range[0]:
                self.dx = -self.dx
                if y > Range[1] or y < Range[0]:
                    self.dy = -self.dy
                Object.Move( (self.dx,self.dy) )
                Object.Text.Move( (self.dx,self.dy))
                self.Canvas.Draw()
                self.TimeStep += 1
                wx.GetApp().Yield(True)
            else:
                self.Timer.Stop()


    def MoveMe(self, Object):
        self.MovingObject = Object
        Range = self.Range
        self.dx = random.uniform(Range[0]/4,Range[1]/4)
        self.dy = random.uniform(Range[0]/4,Range[1]/4)
            #import time
            #start = time.time()
        self.NumTimeSteps = 200
        self.TimeStep = 1
        self.Timer.Start(self.FrameDelay)
            #print "Did %i frames in %f seconds"%(N, (time.time() - start) )
