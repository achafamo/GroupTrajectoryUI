# compgeo-media-2017

Installation
============

Make sure a recent version of the Haskell platform is installed. Anything from
at least 2012 should do. See [1] for downloads and instructions.

The following instructions assume a Linux system

First go to the hgrouping/ directory and type "cabal install" in the terminal. This will download the required packages, but the building phase will fail.

Then go into the hgrouping/src/ directory and type ghc --make HGrouping.hs

[1] http://www.haskell.org/platform/

Basic Usage
===========

Go to the /hgrouping/src/ directory. Here you can execute the program as seen below.

$ ./HGrouping -i <input-file> -o <output-file> -p <epsilon>,<delta>,<m>

### Example 1

A more specific example:

$ ./HGrouping -i ../../input/netlogo/trajectories_300_150.netlogo -o out.ipe -p 10,3.5,3

reads the trajectories from the input file
input/netlogo/trajectories_300_150.netlogo (in netlogo format), computes the
grouping structure with parameters espilon=10, delta=3.5, and m=3, and writes
the groups to a file out.ipe (in Ipe7 file format).


### Example 2

$ ./HGrouping -i ../../input/starkey/starkey_deer.starkey -o out.netlogo -p 10,3.5,3

reads the trajectories from the input file input/starkey/starkey_deer.starkey
(in starkey Input file format), computes the grouping structure with parameters
espilon=10, delta=3.5, and m=3, and writes the groups to a file out.netlogo (in
Netlogo Output file format).


Input/Output Formats
=====================

hgrouping supports several input and output formats specifying the trajectory
and grouping data. The data is selected based on the extension of the file. The formats are:

Ipe7
----

file extension: .ipe

### Input

An Ipe7 [3] file/drawing in which all poly-lines are interpreted as
trajectories. The vertices of a polyine (=trajectory) are assumed to have time stamps 1...tau.

NOTE: It seems that this has broken down after upgrading to a newer version of
hxt. So currently you probably cannot read from the input from an ipe file.

### Output

An Ipe7 file/drawing in which all groups are shown as poly-lines. The `stroke'
style is an indication of the size of the groups.

[3] http://ipe7.sourceforge.net/

Netlogo
-------

file extension: .netlogo

### Input

A plain text format, with on the first line the number of trajectories n, the
second line the length of the trajectories tau. The next tau lines specify the
vertices of trajectory 1, the following tau the vertices of trajectory 2,
etc. The vertex are specified by their x and y coordinates, the time stamps are
assumed to be 1..tau.  i.e.

----- Beginning of file ---------
n
tau
[trajectory1_vertex1_x, trajectory1_vertex1_y]
[trajectory1_vertex2_x, trajectory1_vertex2_y]
[trajectory1_vertex3_x, trajectory1_vertex3_y]
...
[trajectory1_vertextau_x, trajectorytau_vertex1_y]
[trajectory2_vertex1_x, trajectory2_vertex1_y]
..
[trajectory2_vertextau_x, trajectory2_vertextau_y]
...
...
[trajectoryn_vertex1_x, trajectoryn_vertex1_y]
..
[trajectoryn_vertextau_x, trajectoryn_vertextau_y]
------------ End of file -------------

### Output

Information on the numer/size of the ouput groups, and hte parameters used on
the first line (separated by spaces). Then the trajectory data (same as the
input) and finally the groups (one per line). For each group we have: the group
size, its starting time and ending time, and a (space separated) list of ids of
the entities that were in the group.

The ids for each entity is simply a number in the range [1..n], i.e. Entity 1
is the entity corresponding to trajectory1. i.e., if k is the number of groups
that are found:

----- Beginning of file ---------
n tau k eps delta n
[trajectory1_vertex1_x, trajectory1_vertex1_y]
[trajectory1_vertex2_x, trajectory1_vertex2_y]
[trajectory1_vertex3_x, trajectory1_vertex3_y]
...
[trajectory1_vertextau_x, trajectory1_vertextau_y]
[trajectory2_vertex1_x, trajectory2_vertex1_y]
..
[trajectory2_vertextau_x, trajectory2_vertextau_y]
...
...
[trajectoryn_vertex1_x, trajectoryn_vertex1_y]
..
[trajectoryn_vertextau_x, trajectoryn_vertextau_y]
group1_size group1_startTime group1_endTime group1_entity_identifiers
group2_size group2_startTime group2_endTime group2_entity_identifiers
...
groupk_size groupk_startTime groupk_endTime groupk_entity_identifiers
------------ End of file -------------


Starkey
-------

file extension: .starkey

### Input

The .starkey files support trajectories where the vertices do not all have the
same time stamp [4]. The first line contains the number of trajectories, the
second line the number of vertices in each trajectory (seperated by
spaces). The remaining lines all form tripplets specifing vertices. i.e.:


----- Beginning of file ---------
n
tau1 tau2 .... taun
trajectory1_vertex1_x trajectory1_vertex1_y trajectory1_vertex1_t
trajectory1_vertex2_x trajectory1_vertex2_y trajectory1_vertex2_t
trajectory1_vertex3_x trajectory1_vertex3_y trajectory1_vertex3_t
...
trajectory1_vertextau_x trajectorytau_vertex1_y trajectorytau1_vertextau1_t
trajectory2_vertex1_x trajectory2_vertex1_y trajectory2_vertex1_t
..
trajectory2_vertextau_x trajectory2_vertextau_y trajectory2_vertextau2_t
...
...
trajectoryn_vertex1_x trajectoryn_vertex1_y trajectoryn_vertex1_t
..
trajectoryn_vertextau_x trajectoryn_vertextau_y trajectoryn_vertextaun_t
------------ End of file -------------


[4] Note that hgrouping itself will sparsen and synchronize the trajectories:
i.e. produce trajectories that all have the same amount of vertices, s.t. all
trajectoryj_vertexi have the same time stamp t_i.

### Output

undefined. I.e. you cannot use starkey as output format.

JSON
----

### Input

### Output

file extension: .json

Standard Output
===============

The tool will produce a lot of output on the standard output as well. In
particular, the parameters, the groups found, and a text-based representation
of the Reeb graph.

Random notes
============

There should also be code somewhere to produce de Reeb graph in .dot format (so
it can be read/shown using GraphViz).