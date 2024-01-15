#!/bin/bash

for val in $(cat $1)
do
    #/home/projects2/animalia_mito_external/tools/penv/bin/python renameAncestralStates.py /home/projects2/animalia_mito_external/data/${val}/${val}_prank_anc.best.anc.fas /home/projects2/animalia_mito_external/data/${val}/${val}_prank_anc_rename.best.anc.fas
    less /home/projects2/animalia_mito_external/data/${val}/${val}.anc.tree | sed -r "s/(N[0-9]+)/\1${val}/g" > /home/projects2/animalia_mito_external/data/${val}/${val}.new.dnd 
    less /home/projects2/animalia_mito_external/data/${val}/${val}.anc.fa | sed -r "s/(N[0-9]+)/\1${val}/g" > /home/projects2/animalia_mito_external/data/${val}/${val}_renamed.anc.fa
    /home/ctools/vg_1.44.0/bin/vg construct -a -M /home/projects2/animalia_mito_external/data/${val}/${val}_renamed.anc.fa > /home/projects2/animalia_mito_external/data/${val}/${val}_anc.vg
    /home/ctools/vg_1.44.0/bin/vg stats -r /home/projects2/animalia_mito_external/data/${val}/${val}_anc.vg > /home/projects2/animalia_mito_external/data/${val}/posanc.txt
    end_node=$(sed 's/[^,:]*://g' /home/projects2/animalia_mito_external/data/${val}/posanc.txt)
    /home/ctools/vg_1.44.0/bin/vg circularize -a 2 -z $end_node /home/projects2/animalia_mito_external/data/${val}/${val}_anc.vg > /home/projects2/animalia_mito_external/data/${val}/circ_${val}_anc.vg

done
