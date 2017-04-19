#!/bin/sh

#outdir="../../acmgis_2012/figures/netlogo"

fType=".starkey"

indir=$1
outdir=$2

for eps in 100 ; do
    for delta in 500; do
        for m in 7 ; do
            for ff in `ls ${indir}` ; do
                f=$(basename "$ff" "$fType")
                hgrouping -i "${indir}/${ff}" -o "${outdir}/${f}_${eps}_${delta}_${m}.netlogo" -p ${eps},${delta},${m}
            done
        done
    done
done
