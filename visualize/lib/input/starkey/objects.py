import csv

class Entity(object):
    """
    """

    def __init__(self, starkeyId):
        """

        Arguments:
        - `starkeyId`:
        """
        self._starkeyId  = starkeyId
        self._trajectory = dict([])

    def addPoint(self, easting, northing, time):
        """

        Arguments:
        - `easting`:
        - `northing`:
        - `time`:
        """
        (x,y) = (easting,northing)
        self._trajectory[time] = (x,y)
        return self

    def __str__(self):
        return "".join(["{0} {1} {2}\n".format(x,y,t)
                        for (t,(x,y)) in sorted(self._trajectory.items())])

    def start(self):
        return min(self._trajectory.keys())

    def end(self):
        return max(self._trajectory.keys())

    def duration(self):
        return self.end() - self.start()

    def minTime(self):
        return self.start()

    def minX(self):
        return min([x for (x,y) in self._trajectory.values()])

    def minY(self):
        return min([y for (x,y) in self._trajectory.values()])

    def subTime(self,x):
        transform(x,(0,0))

    def translate(self,t):
        transform(0,t)

    def transform(self,time,tr,s = 1):
        def trans(p,t):
            (x,y) = p
            (tx,ty) = t
            return (tx+x,ty+y)

        def scale(s,p):
            (x,y) = p
            return (s*x,s*y)

        self._trajectory = dict([(t+time,scale(s,trans(tr,p))) for (t,p) in
                                 self._trajectory.items()])

    def filteredTimes(self,f):
        return dict([(t,p) for (t,p) in self._trajectory.items() if f(t)])

    def clampToInterval(self,start,end):
        ent  = Entity(self._starkeyId)
        ent._trajectory = self.filteredTimes(lambda t : start <= t and t <= end)
        return ent

    def length(self):
        return len(self._trajectory)


    def isElk(self):
        return self._starkeyId[0] == '9'


    def isDeer(self):
        return self._starkeyId[0] == '8'

    def isCattle(self):
        return self._starkeyId[0] == 'O'


def transformEntities(entities):
    minT  = min([x.minTime() for x in entities])
    minX  = min([x.minX() for x in entities])
    minY  = min([x.minY() for x in entities])
    trans = (-1*minX,-1*minY)
    scale = 1

    # update the timestamps and positions
    for x in entities:
        x.transform(-1*minT,trans,scale)
#        x.filteredTimes(lambda t: t < 100915)

def entitiesFromFile(path, raw = False):
    entities = dict([])
    reader   = csv.DictReader(open(path,'rw'))

    for l in reader:
        (sid,t,e,n) = (l[' Id'], long(l[' StarkeyTime']),
                       int(l[' UTMGridEast']),int(l[' UTMGridNorth']))

        x = entities.get(sid,Entity(sid))
        entities[sid] = x.addPoint(e,n,t)

    if not raw:
        transformEntities(entities.values())
    return entities.values()
