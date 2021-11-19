ssh <user>@stampede2.tacc.utexas.edu
cd $WORK
idev
module list
module load tacc-singularity
singularity pull docker://audiracmichelle/geospatial_argparse
singularity shell geospatial_latest.sif 
singularity exec geospatial__argparse_latest.sif Rscript --vanilla jobs/extract_pm25.R -r 2013.tif -n 60 -c 59 -o "test/" 
