#!/bin/sh

#outdir="../../acmgis_2012/figures/netlogo"

indir=$1 # may also be a file
outdir=$2

for eps in 3 5.25 7.5 ; do
    for delta in 40 100 200; do
        for m in 4 10 17; do
            for ff in `ls ${indir}` ; do
                f=$(basename "$ff" ".netlogo")
                hgrouping -i "${ff}" -o "${outdir}/${f}_${eps}_${delta}_${m}.netlogo" -p ${eps},${delta},${m}
            done
        done
    done
done
