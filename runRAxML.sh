#!/bin/bash

for name in $(cat $1);
do
	/home/ctools/RAxML-8.2.12/raxmlHPC-PTHREADS -s /home/projects2/animalia_mito_external/data/${name}/${name}_prank.best.fas -m GTRCATX  -n ${name} -w /home/projects2/animalia_mito_external/data/${name}/ -p 76 -T 40 -d --HKY85
done


