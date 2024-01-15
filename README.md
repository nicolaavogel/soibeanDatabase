# soibean database

## Pipeline
1. Create a multiple sequence alignment (MSA) with PRANK (Loytynoja and Russell, 2014) from input sequences using the ```run_prank_ford.sh``` script.
2. Build a phylogenetic guide tree based on the HKY substitution model (Hasegawa et al. 1985) with RAxML (Stamatakis 2014). Use ```runRAxML.sh```.
3. Use the MSA and the phylogenetic guide tree as input to reconstruct the input ancestral sequences with FastML (Ashkenazy et al. 2012). Use ```get_anc_states.sh```.
4. Pangenome graph construction can be done with the ```getVGwithanc.sh``` script using the vg toolkit (Garrison et al. 2018).
5. To combine multiple pangenome graphs the node ID index needs to be joined using the ```newvgmanipIDs.sh``` script.
6. Combination of all ID-adjusted graphs can be done using the ```vgcombinewithancNew.sh``` script.

Please note that all scripts need to be executable using ```chmod +x <script.sh>```. Additionally, please adjust the absolute paths used in the scripts to fit the location of the tools.

## Test data
All simulated FASTQ files used in the publication are in the ```test data `` folder. Parameters used for the specific FASTQ files can be found in the publication. 

## Visualization 
All visualization scripts can be found in the ```RPlottingScripts``` folder. 


## References
