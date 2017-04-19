
from math import cos,sin,atan2,pi,hypot

from os import listdir
from os.path import join


from objects import *

def linearinterp((px,py),(qx,qy),alpha):
    def linear(p,q,alpha):
        return (1-alpha)*p + alpha*q
    return (linear(px,qx,alpha),linear(py,qy,alpha))


# p - q
def minp((px,py),(qx,qy)):
    return (px-qx,py-qy)

# euclidean distance
def dist(p,q):
    """
    """
    return hypot(*minp(q,p))




# find a point very close to q that lies on the line pq
def findclosepoint(p,q):
    def f(p,q):
        return (q - 1 - p) / (q - p) if p != q else None
    alpha = f(p[0],q[0])
    return (q[0],q[1]-1) if alpha is None else linearinterp(p,q,alpha)


def tp(s):
    """
    """
    [x,y,t] = s.split(' ')
    return (float(x),float(y),int(round(float(t))))


def fromDir(path):
    return [fromFile(join(path,f)) for f in listdir(path) if f[0] != '.']


def fromFile(path):
    """
    """
    f = open(path,'r')
    lines = f.readlines()
    f.close()

    meta = lines[0].split(' ')
    [n,tau,ng,m] = map(int,meta[:3]+meta[5:])
    [eps,delta]  = map(float,meta[3:5])


    entities = []
    lines    = lines[1:]
    for i in range(0,n):
        t = Trajectory([tp(l) for l in lines[:tau]])
        entities.append(Entity(i+1,t))
        lines = lines[tau:]

    # convert to dictionary
    entities = dict([(e.tid,e) for e in entities])

    groups = []
    for i in range(0,ng):
        info           = lines[0].split(' ')
        members        = [entities[int(x)] for x in lines[1].split(' ')]
        lines          = lines[2:]
        groups.append(Group(i+1,members,float(info[1]),float(info[2])))

    # reverse the groups, so they are increasing in size (from small to large)
    # this will make sure we print the small ones first and the large ones last
    groups.reverse()

    return GroupingResult(entities,groups,eps,delta,m)


def fromFileIt(path):
    """
    """
    f = open(path,'r')
    lines = f.readlines()
    f.close()

    meta = lines[0].split(' ')
    [n,tau,ng,m] = map(int,meta[:3]+meta[5:])
    [eps,delta]  = map(float,meta[3:5])


    entities = []
    lines    = lines[1:]
    for i in range(0,n):
        t = Trajectory([tp(l) for l in lines[:tau]])
        entities.append(Entity(i+1,t))
        lines = lines[tau:]

    # convert to dictionary
    entities = dict([(e.tid,e) for e in entities])

    groups = []
    for i in range(0,ng):
        info           = lines[0].split(' ')
        members        = [entities[int(x)] for x in lines[1].split(' ')]
        lines          = lines[2:]
        groups.append(Group(i+1,members,float(info[1]),float(info[2])))

    return GroupingResult(entities,groups,eps,delta,m)
