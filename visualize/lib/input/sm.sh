#!/bin/sh

outdir="../../acmgis_2012/figures/smallmultiples"

for eps in 10 20 ; do
    for delta in 2 3.5 4 5 ; do
        for m in 2 3 5 ; do
            hgrouping -i acmgis.ipe -o "${outdir}/groups_${eps}_${delta}_${m}.ipe" -e ${eps} -d ${delta} -m ${m}
        done
    done
done
