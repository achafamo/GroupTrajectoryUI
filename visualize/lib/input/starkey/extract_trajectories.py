#!/usr/local/bin/python

import sys

from objects import *

activeRanges = [ (2.87e7, 4e7    )
               , (3.74e7, 4e7    )
               , ( 6e7  , 7.195e7)
               , (9.4e7 , 10.3e7)
               ]


def generateEntitiesInRange(r,kind, entities):
    """
    r       : Input range
    kind    : the input kind we are interested in
    entities: all entities

    result  : trajectories of all active entities of the given kind in the given input range
    """

    def inOutput(x):
        d = { 'deer'   : x.isDeer
            , 'elk'    : x.isElk
            , 'cattle' : x.isCattle
            , 'all'    : lambda:True
            }
        return d[kind]() and x.length() > 0

    entities = [x.clampToInterval(*r) for x in entities]

    return [x for x in entities if inOutput(x)]


def writeOut(outFile,entities):
    with open(outFile,'w') as f:
        f.write(str(len(entities)) + "\n")
        f.write(" ".join([str(x.length()) for x in entities]) + "\n")
        f.writelines([str(x) for x in entities])


def main(args):

    kind       = args[1] # one of: deer, elk, cattle, all
    inFile     = args[2]
    outPattern = args[3] # initial part of the output file (path+partial filename)

    entities = entitiesFromFile(inFile)

    for r in activeRanges:
        outName = outPattern + ("range_{0}_{1}.starkey".format(*r))
        writeOut(outName, generateEntitiesInRange(r,kind, entities))








if __name__ == '__main__':
    main(sys.argv)
