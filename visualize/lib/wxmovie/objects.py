# class GroupingResults(object):
#     """
#     """

#     def __init__(self, results):
#         """

#         Arguments:
#         - `entities`:
#         - `results`:
#         """
#         self._entities = results[0]._entities
#         self._results = [GroupingResult([],r.groups,r.eps,r.delta,r.m) for ]


import bisect

class GroupingResult(object):
    """
    """

    def __init__(self, entities, groups, eps, delta, m):
        """

        Arguments:
        - `entities`:
        - `groups`:
        - `eps`:
        - `delta`:
        - `m`:
        """
        self._entities = entities
        self._groups = groups
        self._epsilon = eps
        self._delta = delta
        self._m = m




class Trajectory(object):
    """
    """

    def __init__(self, timepoints):
        """

        Arguments:
        - `timepoints`:
        """
        self.timepoints = dict([(t,(x,y)) for (x,y,t) in timepoints])
        self.computeTimes()
        self._numVertices = len(self.timepoints.keys())


    def computeTimes(self):
        self.times = sorted(self.timepoints.keys())


    @classmethod
    def fromdict(self, tps):
        t = Trajectory([])
        t.timepoints = tps
        t.computeTimes()
        return t

    def at(self,t):
        return self.timepoints[t]

    def length(self):
        return self._numVertices

    def positions(self):
        return [p for (t,p) in sorted(self.timepoints.items(),key=lambda (k,v): k)]

    def subtrajectory(self,start,end):
        return Trajectory.fromdict(dict([(t,p) for (t,p) in self.timepoints.items()
                                         if start <= t and t <= end]))


    def pred(self,t):
        i = bisect.bisect_left(self.times, t)
        j = (i-1) if i else 0
        return self.times[j]

    def succ(self,t):
        i = bisect.bisect_right(self.times,t)
        j = i     if i != self._numVertices else (-1)
        return self.times[j]


class Entity(object):
    """
    """

    def __init__(self, tid, trajectory):
        """

        Arguments:
        - `tid`:
        - `trajectory`:
        """
        self.tid = tid
        self.trajectory = trajectory

    def __eq__(self,o):
        return self._tid == o._tid

    def __neq__(self,o):
        return not (self == 0)


    def position(self, t):
        """

        Arguments:
        - `t`:
        """
        return self.trajectory.at(t)

    def prevT(self,t):
        return self.trajectory.pred(t)

    def nextT(self,t):
        return self.trajectory.succ(t)



class Group(object):
    """
    """

    def __init__(self, gid, members, start, end):
        """

        Arguments:
        - `gid`:
        - `members`:
        - `start`:
        - `end`:
        """
        self.gid = gid
        self.members = members
        self.start = start
        self.end = end

    def size(self):
        return len(self.members)


    def duration(self):
        return self.end - self.start

    def isActive(self, t):
        """

        Arguments:
        - `t`:
        """
        return self.start <= t and t <= self.end
