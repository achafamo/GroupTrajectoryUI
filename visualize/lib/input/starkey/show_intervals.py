import sys

from objects import *
import numpy as np
import matplotlib.pyplot as plt

def main(args):
    inFile = args[1]

    entities = entitiesFromFile(inFile)

    starts    = [x.start() for x in entities]
    durations = [x.duration() for x in entities]

    # starts    = [5, 10, 15]
    # durations = [20, 10, 15]

    zeros     = np.zeros(len(starts))
    ents      = list(range(1,len(starts)+1))

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.errorbar(starts, ents, xerr=[zeros,durations],linestyle='')
    plt.show()

if __name__ == '__main__':
    main(sys.argv)
