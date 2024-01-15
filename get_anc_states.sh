#!/bin/bash

for name in $(cat $1)
do
    /home/ctools/FastML.v3.11/programs/fastml/fastml -s /home/projects2/animalia_mito_external/data/${name}/${name}_prank.best.fas -t /home/projects2/animalia_mito_external/data/${name}/RAxML_bestTree.${name} -x /home/projects2/animalia_mito_external/data/${name}/${name}.anc.tree -j /home/projects2/animalia_mito_external/data/${name}/${name}.anc.fa -mr -mh -qf > /home/projects2/animalia_mito_external/data/${name}/${name}.log:
    #sed -i 's/^>N\([0-9]\+\)/>N\1${name}/' ${name}.anc.tree > ${name}.new.dnd
    #sed -i 's/^>N\([0-9]\+\)/>N\1${name}/' ${name}.anc.fa > ${name}_renamed.anc.fa
done
