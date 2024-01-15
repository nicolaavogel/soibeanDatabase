#!/bin/bash

for val in $(cat taxalist2);
do
        zcat /home/projects2/animalia_mito_external/data/${val}/${val}_for_msa.new.fa.gz | /usr/bin/time -v /home/ctools/prank-msa/src/prank -d=/dev/stdin -o=/home/projects2/animalia_mito_external/data/${val}/${val}_prank -showall -DNA
done
