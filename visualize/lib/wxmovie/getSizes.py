import sys

from objects     import *
from common      import *

import itertools

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab



def getStats(path):
    """
    """
    gr       = fromFile(path)
    grs = gr._groups
    sizes     = [g.size()     for g in grs]
    durations = [g.duration() for g in grs]
    return (sizes,durations)

# (min(sizes),max(sizes),min(durations),max(durations))

def main(args):
    """
    """

    def flatten(xs) :
        return list(itertools.chain(*xs))

    inFiles = args[1:]

    sizes,durations = zip(*[getStats(f) for f in inFiles])
    print sizes

    sizeP = plt.figure().add_subplot(111)
    sizeP.hist(flatten(sizes), bins=10, normed=1, facecolor='green', alpha=0.75)
    plt.show()

    durs  = plt.figure().add_subplot(212)
    durs.hist(flatten(durations), bins=10, normed=1, facecolor='blue', alpha=0.75)

    plt.show()

    # print "minSize:" + str(min(getI(stats,0)))
    # print "maxSize:" + str(max(getI(stats,1)))
    # print "minDuration:" + str(min(getI(stats,2)))
    # print "maxDuration:" + str(max(getI(stats,3)))


if __name__ == '__main__':
    main(sys.argv)
