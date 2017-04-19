#!/bin/sh

outdir="/home/cosc301/Desktop/Trajectory/videos/"

indir="/home/cosc301/Desktop/Trajectory/hgrouping/outputs/out_small.netlogo"
#outdir=$2

wxmovie="python2 /home/cosc301/Desktop/Trajectory/wxmovie/wxmovie.py"

#for ff in $(find . -wholename "./${indir}*") ; do
#    f=$(basename "$ff" ".netlogo")
#    ${wxmovie} "${ff}" "${outdir}/${f}.mkv" $3 $4 $5 $6 $7 $8
#done

${wxmovie} ${indir} ${outdir}/test.mkv