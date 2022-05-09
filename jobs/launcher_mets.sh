#!/bin/bash
#SBATCH -J mets                     # job name
#SBATCH -N 1                        # number of nodes requested
#SBATCH -n 36                       # total number of tasks to run in parallel
#SBATCH -p skx-dev                  # queue (partition) 
#SBATCH -t 00:45:00                 # run time (hh:mm:ss) 

module load tacc-singularity
module load launcher

export LD_PRELOAD=""
export LAUNCHER_WORKDIR=/work/08317/m1ch3ll3/stampede2/pm25_grid_zip
export LAUNCHER_JOB_FILE=jobs/jobs_mets

${LAUNCHER_DIR}/paramrun
